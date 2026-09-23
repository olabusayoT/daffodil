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

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

import org.junit.Assert.*
import org.junit.Test

/**
 * Proves unparsePendingSuspensionTripLimit is an independent bound from
 * prefetchLimit: pendingCount alone must still trip an early write catch-up.
 */
class PendingSuspensionTripLimitTest {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  private def testSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
    <xs:element name="root" dfdl:lengthKind="implicit">
      <xs:complexType>
        <xs:sequence>
          <xs:element name="record" maxOccurs="unbounded">
            <xs:complexType>
              <xs:sequence dfdl:separator="|">
                <xs:element
                  name="len"
                  type="xs:int"
                  dfdl:lengthKind="explicit"
                  dfdl:length="4"
                  dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
                <xs:element name="data" type="xs:string" dfdl:lengthKind="delimited"/>
              </xs:sequence>
            </xs:complexType>
          </xs:element>
        </xs:sequence>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  @Test def testPendingCountTripsEarlyWriteCatchUp(): Unit = {
    val numRecords = 40
    val pendingSuspensionTripLimit = 3L
    val prefetchLimit = 10000L // unreachably large: isolates pendingSuspensionTripLimit

    val sch = testSchema
    val records = (0 until numRecords).map(i => <record><data>{s"value$i"}</data></record>)
    val infoset = <ex:root xmlns:ex={example}>{records}</ex:root>

    val singlePassDp = TestUtils.compileForUnparse(sch)
    val singlePassOut = new ByteArrayOutputStream()
    val singlePassRes =
      singlePassDp.unparse(new ScalaXMLInfosetInputter(infoset), singlePassOut)
    assertFalse(singlePassRes.getDiagnostics.toString, singlePassRes.isError)
    val expectedBytes = singlePassOut.toByteArray

    val dp = TestUtils.compileForUnparse(
      sch,
      Map(
        "releaseUnneededInfoset" -> "false",
        "unparsePendingSuspensionTripLimit" -> pendingSuspensionTripLimit.toString
      )
    )

    val buildInputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(buildInputter.documentElement, dp, prefetchLimit)()

    val walkerOut = new ByteArrayOutputStream()
    val writeInputter = TestUtils.newInitializedInputter(infoset, dp)
    val writeState = UState.createInitialUStateForSharedVariables(
      walkerOut,
      dp,
      writeInputter,
      sharedCtx.variableBox,
      false
    )
    writeState.setSharedContext(sharedCtx)
    writeState.getDataOutputStream.setPriorBitOrder(dp.ssrd.elementRuntimeData.defaultBitOrder)

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    UnparseSharedContextTestFixture.wireCoroutines(
      sharedCtx,
      buildInputter.documentElement,
      rootUnparser,
      writeState
    )

    val buildState = new BuildState(buildInputter, sharedCtx, Nil, false)
    buildState.initializeVariables()

    dp.ssrd.unparser.unparse1(buildState)

    // numRecords*3 + 1 (root, record, len, data per record) is what
    // currentLead would equal if pendingSuspensionTripLimit weren't wired in.
    assertTrue(
      s"expected lead bounded well below the full tree via pendingSuspensionTripLimit=" +
        s"$pendingSuspensionTripLimit tripping an early write catch-up (prefetchLimit=$prefetchLimit " +
        s"alone never would), but lead was ${sharedCtx.currentLead} (numRecords=$numRecords)",
      sharedCtx.currentLead < numRecords
    )
    assertTrue(
      "expected build to have gotten ahead of write by at least one node",
      sharedCtx.currentLead > 0
    )

    val finalSignal = sharedCtx.resumeWrite(BuildFinished)
    finalSignal match {
      case WriteDone(Some(t)) => throw t
      case WriteDone(None) => // continue below
      case other => fail(s"unexpected final signal: $other")
    }
    writeState.evalSuspensions(isFinal = true)
    writeState.getDataOutputStream.setFinished(writeState)

    assertArrayEquals(expectedBytes, walkerOut.toByteArray)
  }
}
