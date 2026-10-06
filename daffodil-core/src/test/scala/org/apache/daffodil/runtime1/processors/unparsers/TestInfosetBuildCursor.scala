/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.daffodil.runtime1.processors.unparsers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.xml.Node

import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.InfosetBuildCursor
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

import org.junit.Assert.*
import org.junit.Test

/**
 * Tests the infoset build cursor and build state against the unparseTree
 * pass: the event sequence build surfaces, the shared lead counter, the
 * prefetch window, and unparseTree matching a single-pass unparse for scalar,
 * array and choice content.
 */
class TestInfosetBuildCursor {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  // A separated sequence of three scalars.
  private val separatedRowSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"
      encoding="ascii"
      lengthUnits="bytes"
      outputNewLine="%CR;%LF;"/>,
    <xs:element name="row" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
          <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="age" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="city" type="xs:string" dfdl:lengthKind="delimited"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  private val separatedRowInfoset =
    <ex:row xmlns:ex={example}>
      <name>Alice</name>
      <age>30</age>
      <city>Boston</city>
    </ex:row>

  // A scalar, an array and a choice in one separated sequence.
  private val arrayChoiceSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"
      encoding="ascii"
      lengthUnits="bytes"/>,
    <xs:element name="row" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
          <xs:element name="header" type="xs:string" dfdl:lengthKind="delimited"/>
          <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
            dfdl:lengthKind="delimited"
            dfdl:occursCountKind="implicit"/>
          <xs:choice>
            <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:choice>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  // Three "item" occurrences exercise the array loop; typeB (not typeA)
  // exercises actual choice resolution. lengthKind=delimited (not explicit)
  // avoids double-firing CaptureStartOfContentLengthUnparser's non-idempotent
  // marker, since a tree reused from a completed unparse runs it again.
  private val arrayChoiceInfoset =
    <ex:row xmlns:ex={example}>
      <header>H</header>
      <item>a</item>
      <item>b</item>
      <item>c</item>
      <typeB>X</typeB>
    </ex:row>

  /**
   * A build side, an InfosetBuildCursor over an InfosetBuildState, and an
   * unparseTree side sharing one UnparseSharedContext.
   */
  private final class PrefetchRun(sch: Node, infoset: Node, prefetchLimit: Long = 100) {
    val dp = UnparseSharedContextTestFixture.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildPrefetch" -> "true")
    )
    val buildInputter = UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)
    val sharedCtx = UnparseSharedContextTestFixture.build(dp, prefetchLimit)()
    val cursor = new InfosetBuildCursor(
      dp.ssrd.builder,
      new InfosetBuildState(buildInputter, sharedCtx),
      sharedCtx
    )
    sharedCtx.setBuildCursor(cursor)

    def buildAll(): Unit = cursor.advance(lastAdvance = true)

    def rootNode = buildInputter.documentElement.child(0).asComplex

    // Unparses the tree build produced, pulling build forward as unparseTree
    // needs it, and returns the output.
    def unparseTree(): String = {
      val out = new ByteArrayOutputStream()
      val inputter = UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)
      val state = UState.createInitialUState(out, dp, inputter, false)
      state.setSharedContext(sharedCtx)
      state.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

      val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
      rootUnparser.unparseTree(sharedCtx.awaitChild(buildInputter.documentElement, 0), state)
      // Build may still have trailing end events to consume, and a speculative
      // separator is written via a suspension that must drain before the DOS
      // is finalized.
      buildAll()
      state.evalSuspensions(isFinal = true)
      state.getDataOutputStream.setFinished(state)
      new String(out.toByteArray, StandardCharsets.US_ASCII)
    }
  }

  // A tree built outside a single pass, compared against that single pass.
  private def assertUnparseTreeMatchesSinglePass(sch: Node, infoset: Node): Array[Byte] = {
    // Single-pass on purpose: the tunable would otherwise replace it.
    val dp = UnparseSharedContextTestFixture.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildPrefetch" -> "false")
    )
    val (singlePassBytes, unparseTreeBytes) =
      UnparseSharedContextTestFixture.getSinglePassAndUnparseTreeBytes(dp, infoset)
    assertArrayEquals(singlePassBytes, unparseTreeBytes)
    singlePassBytes
  }

  @Test def testBuildStateSurfacesCorrectEventSequence(): Unit = {
    val run = new PrefetchRun(separatedRowSchema, separatedRowInfoset)

    // Drives through the actual InfosetBuilder frames rather than hand-driven
    // advance() calls, since next-element resolution depends on the TRD
    // push/pop the element frame performs. This schema's separator never
    // reaches InfosetBuildState: the InfosetBuilder tree skips the
    // delimiter-stack wrapper.
    run.buildAll()

    assertEquals(4L, run.sharedCtx.currentLead) // row, name, age, city

    assertEquals(3, run.rootNode.numChildren)
    assertEquals("name", run.rootNode.child(0).erd.name)
    assertEquals("age", run.rootNode.child(1).erd.name)
    assertEquals("city", run.rootNode.child(2).erd.name)
  }

  @Test def testLeadCounterIncrementsOnBuildAndDecrementsOnUnparse(): Unit = {
    // Fixed dfdl:length is safe here because this tree comes from
    // InfosetBuildState, which never runs content-unparsing (including
    // CaptureStartOfContentLengthUnparser).
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="name" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="5"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="6"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val run = new PrefetchRun(sch, separatedRowInfoset)

    assertEquals(0L, run.sharedCtx.currentLead)
    run.buildAll()
    // row itself, name, age, city = 4 elements total, each incrementing once
    // via unparseBegin's actual hookup.
    assertEquals(4L, run.sharedCtx.currentLead)

    // unparseTree unparses the same already-built tree against the same
    // shared context, decrementing the lead counter as it goes.
    assertEquals("Alice30Boston", run.unparseTree())

    // unparseTree decremented once per element too, so the counter is back to
    // 0: build and unparseTree agree on how many nodes exist.
    assertEquals(0L, run.sharedCtx.currentLead)
  }

  // With a small prefetchLimit, one advance() leaves the lead counter just
  // past it, and the unparse is still correct across the refills unparseTree
  // triggers while it runs.
  @Test def testBuildStopsAtPrefetchLimit(): Unit = {
    val numItems = 40
    val prefetchLimit = 3L

    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val items = (0 until numItems).map(i => <item>{s"i$i"}</item>)
    val infoset =
      <ex:row xmlns:ex={example}>
        {items}
      </ex:row>
    val expectedBytes = (0 until numItems).map(i => s"i$i").mkString(",")

    val run = new PrefetchRun(sch, infoset, prefetchLimit)

    run.cursor.advance()

    // numItems + 1 (row + all items) is what currentLead would equal here if
    // advance() ignored the prefetch limit. It counts one node per element, so
    // it stops at the first node past the limit.
    assertFalse("expected build to stop with more left to build", run.cursor.isFinished)
    assertEquals(
      s"expected build to stop as soon as the lead passed prefetchLimit=$prefetchLimit " +
        s"(numItems=$numItems)",
      prefetchLimit + 1,
      run.sharedCtx.currentLead
    )

    // unparseTree pulls the rest of the tree forward as it needs it; the
    // output must be byte-for-byte correct despite having been built across
    // many separate advance() calls rather than a single one-shot build pass.
    assertEquals(expectedBytes, run.unparseTree())
  }

  @Test def testUnparseTreeMatchesSinglePass(): Unit = {
    assertUnparseTreeMatchesSinglePass(separatedRowSchema, separatedRowInfoset)
  }

  @Test def testArrayAndChoiceUnparseTreeMatchesSinglePass(): Unit = {
    val singlePassBytes =
      assertUnparseTreeMatchesSinglePass(arrayChoiceSchema, arrayChoiceInfoset)
    assertEquals("H,a,b,c,X", new String(singlePassBytes, StandardCharsets.US_ASCII))
  }

  // Drives InfosetBuildState directly, then feeds its tree to unparseTree
  // (end-to-end build-then-unparseTree).
  @Test def testStandaloneBuildStateNavigatesArrayChoiceSeparator(): Unit = {
    val run = new PrefetchRun(arrayChoiceSchema, arrayChoiceInfoset)

    // The cursor builds the whole tree from the inputter, including the array
    // and choice content.
    run.buildAll()

    // row, header, item x3, typeB = 6 elements total.
    assertEquals(6L, run.sharedCtx.currentLead)

    assertEquals(3, run.rootNode.numChildren)
    assertEquals("header", run.rootNode.child(0).erd.name)
    assertEquals("item", run.rootNode.child(1).erd.name)
    assertEquals(3, run.rootNode.child(1).asInstanceOf[DIArray].numChildren)
    assertEquals("typeB", run.rootNode.child(2).erd.name)

    // Unparsing the tree InfosetBuildState just constructed confirms it's a
    // usable, fully-built tree, not just a navigation exercise.
    assertEquals("H,a,b,c,X", run.unparseTree())
  }
}
