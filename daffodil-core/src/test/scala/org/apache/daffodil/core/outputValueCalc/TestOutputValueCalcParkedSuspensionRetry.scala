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

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Test

/**
 * Regression guard for SuspensionTracker.evalParkedSuspensions' churn-
 * reduction rework (suspensionsParked is no longer drained up front every
 * force-retry pass): a dfdl:valueLength forward reference spanning many
 * intervening records forces the referencing element's suspension to
 * survive many force-retry cycles before its target is finally written,
 * exactly the shape that rework targets. unparseSuspensionWaitOld/Young
 * are set to 1 so every single unparsed element triggers a force-retry
 * pass, maximizing how many times each still-pending suspension is
 * cycled through before the test schema's small document finishes.
 *
 * len3/len4 both reference the SAME record (the last one), so they share
 * one target LengthState's SuspensionWaiter and must both be notified
 * off a single registered-suspension-count-greater-than-one waiter when
 * it finally resolves - the shared-waiter shape a reentrant notify during
 * a force-retry pass depends on.
 */
class TestOutputValueCalcParkedSuspensionRetry {

  private val example = XMLUtils.EXAMPLE_NAMESPACE

  private val N = 12

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
                  <xs:element name="len1" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[4]/data, 'bytes') }"
      }/>
                  <xs:element name="len2" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[8]/data, 'bytes') }"
      }/>
                  <xs:element name="len3" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$N]/data, 'bytes') }"
      }/>
                  <xs:element name="len4" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="4"
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

  // Every len field is a fixed-length "data" element (8 bytes), so all
  // four should compute to 8 regardless of which record they target.
  private def expectedOutput = {
    val header = "   8   8   8   8"
    val records = (1 to N).map(i => f"T$i%03d" + f"data$i%04d").mkString
    header + records
  }

  @Test def testManyForceRetryCyclesResolveCorrectly(): Unit = {
    TestUtils.testUnparsing(
      schema,
      infoset,
      expectedOutput,
      tunables = Map("unparseSuspensionWaitOld" -> "1", "unparseSuspensionWaitYoung" -> "1")
    )
  }

  @Test def testManyForceRetryCyclesDefaultTunablesStillMatch(): Unit = {
    // Same schema/infoset at the DEFAULT unparseSuspensionWaitOld/Young
    // (100/5): confirms the aggressive-tunable test above isn't passing
    // for a reason unrelated to the tunable (e.g. a schema mistake), by
    // producing byte-identical output under ordinary throttling.
    TestUtils.testUnparsing(schema, infoset, expectedOutput)
  }
}
