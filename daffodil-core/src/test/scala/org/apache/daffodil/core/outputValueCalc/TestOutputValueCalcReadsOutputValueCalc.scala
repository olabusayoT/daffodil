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

package org.apache.daffodil.core.outputValueCalc

import java.nio.channels.Channels

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.DataProcessor

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard for the real production hand-off in
 * Suspension.moveFromParkedToYoung (savedUstate.suspensionTracker.
 * moveParkedToYoung): lenA's outputValueCalc reads lenB's own value, an
 * OutputValueCalcEvaluationException/DISimple.suspensionWaiter wake-up
 * distinct from the LengthState-based one
 * TestOutputValueCalcParkedSuspensionRetry already exercises.
 * unparseSuspensionWaitOld is set far larger than the total number of
 * suspension ticks this tiny document can ever produce, so
 * evalParkedSuspensions' unconditional force-retry fallback never runs a
 * second time: the only way either suspension can resolve within this test
 * is a real registered wake-up firing and hand off through that line.
 */
class TestOutputValueCalcReadsOutputValueCalc {

  private val example = XMLUtils.EXAMPLE_NAMESPACE

  private val N = 3

  private def schema = {
    val record =
      <xs:element
        name="record"
        minOccurs={N.toString}
        maxOccurs={N.toString}
        dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
            <xs:element name="tag" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="4"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="8"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
    SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" textStringJustification="left" textPadKind="padChar" textStringPadCharacter="%SP;"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
            <xs:element name="header" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="lenA" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                    dfdl:outputValueCalc="{ ../lenB }"/>
                  <xs:element name="lenB" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$N]/data, 'bytes') }"
      }/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            {record}
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
  }

  private def infoset = {
    val recordXml = (1 to N).map { i =>
      <record><tag>{f"T$i%03d"}</tag><data>{f"data$i%04d"}</data></record>
    }
    <ex:root xmlns:ex={example}>
      <header/>
      {recordXml}
    </ex:root>
  }

  private def compile(tunables: Map[String, String]): DataProcessor = {
    val compiler = Compiler().withTunables(tunables)
    val pf = compiler.compileNode(schema)
    if (pf.isError) fail(pf.getDiagnostics.toString)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) fail(dp.getDiagnostics.toString)
    dp
  }

  @Test def testOvcReadsOvcResolvesViaRealTargetedWakeup(): Unit = {
    val dp = compile(Map("unparseSuspensionWaitOld" -> "1000000"))

    val outputStream = new java.io.ByteArrayOutputStream()
    val out = Channels.newChannel(outputStream)
    val inputter = new ScalaXMLInfosetInputter(infoset)
    val actual = dp.unparse(inputter, out)
    out.close()
    assertFalse(actual.getDiagnostics.toString, actual.isProcessingError)

    val unparsed = outputStream.toString
    // lenA reads lenB's own value (not its length): both should equal
    // record[N]/data's length, 8.
    assertEquals("   8   8", unparsed.substring(0, 8))

    val recordsPart = unparsed.substring(8)
    val expectedRecords = (1 to N).map(i => f"T$i%03d" + f"data$i%04d").mkString
    assertEquals(expectedRecords, recordsPart)
  }
}
