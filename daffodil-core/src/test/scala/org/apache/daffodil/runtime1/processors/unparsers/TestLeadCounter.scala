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

import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.InfosetBuildCursor
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates the shared build/unparse lead counter end to end: build
 * increments and unparse decrements against the same shared instance.
 */
class TestLeadCounter {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testLeadCounterIncrementsOnBuildAndDecrementsOnUnparse(): Unit = {
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

    // Fixed dfdl:length is safe here because this tree comes from
    // InfosetBuildState, which never runs content-unparsing (including
    // CaptureStartOfContentLengthUnparser).
    val infoset =
      <ex:row xmlns:ex={example}>
        <name>Alice</name>
        <age>30</age>
        <city>Boston</city>
      </ex:row>

    val dp = UnparseSharedContextTestFixture.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildPrefetch" -> "true")
    )

    // Build pass: drive InfosetBuildState through an InfosetBuildCursor, incrementing the
    // shared lead counter via the actual unparseBegin hookup.
    val buildInputter = UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit = 100)()
    val infosetBuildState =
      new InfosetBuildState(buildInputter, sharedCtx, areDebugging = false)

    assertEquals(0L, sharedCtx.currentLead)
    new InfosetBuildCursor(dp.ssrd.builder, infosetBuildState, sharedCtx).runToCompletion()

    // row itself, name, age, city = 4 elements total, each incrementing
    // once via unparseBegin's actual hookup.
    assertEquals(4L, sharedCtx.currentLead)

    // unparseTree pass: unparse the SAME already-built tree
    // (buildInputter.documentElement) against the SAME sharedCtx,
    // decrementing the lead counter as it goes.
    val unparseTreeOut = new ByteArrayOutputStream()
    val unparseTreeInputter =
      UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)
    val unparseTreeState =
      UState.createInitialUState(unparseTreeOut, dp, unparseTreeInputter, false)
    unparseTreeState.setSharedContext(sharedCtx)
    unparseTreeState.getDataOutputStream.setPriorBitOrder(
      dp.ssrd.elementRuntimeData.defaultBitOrder
    )

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    val rootNode = sharedCtx.awaitChild(buildInputter.documentElement, 0)
    rootUnparser.unparseTree(rootNode, unparseTreeState)
    unparseTreeState.getDataOutputStream.setFinished(unparseTreeState)

    assertEquals(
      "Alice30Boston",
      new String(unparseTreeOut.toByteArray, StandardCharsets.US_ASCII)
    )

    // unparseTree decremented once per element too, so the counter is back to 0:
    // build and unparseTree agree on how many nodes exist, coordinated through
    // the shared UnparseSharedContext.
    assertEquals(0L, sharedCtx.currentLead)
  }
}
