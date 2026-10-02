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
 * Proves build stops to let write catch up: with a small prefetchLimit,
 * one advance() leaves the lead counter just past it, and the lead never
 * goes further across the refills write triggers while it runs.
 */
class TestBoundedPrefetch {

  val example = XMLUtils.EXAMPLE_NAMESPACE

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

    val dp = UnparseSharedContextTestFixture.compileForUnparse(
      sch,
      Map("releaseUnneededInfoset" -> "false", "useBuildWritePrefetch" -> "true")
    )

    val buildInputter = UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(dp, prefetchLimit)()

    val walkerOut = new ByteArrayOutputStream()
    val writeInputter = UnparseSharedContextTestFixture.newInitializedInputter(infoset, dp)
    val writeState = UState.createInitialUState(walkerOut, dp, writeInputter, false)
    writeState.setSharedContext(sharedCtx)
    writeState.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]

    val buildState = new InfosetBuildState(buildInputter, sharedCtx)
    val cursor = new InfosetBuildCursor(dp.ssrd.builder, buildState, sharedCtx)
    sharedCtx.setBuildCursor(cursor)

    cursor.advance()

    // numItems + 1 (row + all items) is what currentLead would equal here
    // if advance() ignored the prefetch limit. It counts one node per
    // element, so it stops at the first node past the limit.
    assertFalse("expected build to stop with more left to build", cursor.isFinished)
    assertEquals(
      s"expected build to stop as soon as the lead passed prefetchLimit=$prefetchLimit " +
        s"(numItems=$numItems)",
      prefetchLimit + 1,
      sharedCtx.currentLead
    )

    // Write pulls the rest of the tree forward as it needs it; confirm the
    // output is byte-for-byte correct despite having been built across many
    // separate advance() calls rather than a single one-shot build pass.
    val rootNode = sharedCtx.awaitChild(buildInputter.documentElement, 0)
    rootUnparser.writeContent(rootNode, writeState)
    cursor.runToCompletion()
    writeState.evalSuspensions(isFinal = true)
    writeState.getDataOutputStream.setFinished(writeState)

    assertEquals(expectedBytes, new String(walkerOut.toByteArray, StandardCharsets.US_ASCII))

    // Every refill write triggered stopped at the same point, so the lead
    // never went past one node beyond the window, and did reach it.
    assertEquals(prefetchLimit + 1, sharedCtx.peakLead)
  }
}
