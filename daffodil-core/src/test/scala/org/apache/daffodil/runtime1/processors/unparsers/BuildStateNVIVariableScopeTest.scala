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

import scala.jdk.CollectionConverters.*

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.externalvars.ExternalVariablesLoader
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.VariableBox

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard: after build pops a local NVI scope, a read must
 * still see the top-level value, not a stale leftover from that scope.
 */
class BuildStateNVIVariableScopeTest {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testVariableReadAfterNVIScopeCloses(): Unit = {
    val numRecords = 25
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>, {
        <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>
        <dfdl:defineVariable name="runningVar" type="xs:int" external="true"/>
      },
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="record" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence>
                  <xs:annotation>
                    <xs:appinfo source="http://www.ogf.org/dfdl/">
                      <dfdl:newVariableInstance ref="tns:runningVar" defaultValue="0"/>
                    </xs:appinfo>
                  </xs:annotation>
                  <xs:element name="idx" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2">
                    <xs:annotation>
                      <xs:appinfo source="http://www.ogf.org/dfdl/">
                        <dfdl:setVariable ref="tns:runningVar" value="{ . }"/>
                      </xs:appinfo>
                    </xs:annotation>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element
              name="summary"
              type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="4"
              dfdl:outputValueCalc="{ $tns:runningVar }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val records = (0 until numRecords).map(i => <record><idx>{f"$i%02d"}</idx></record>)
    val infoset = <ex:root xmlns:ex={example}>{records}</ex:root>

    val extVars =
      ExternalVariablesLoader.mapToBindings(Map(s"{$example}runningVar" -> "-1").asJava)

    val pf = Compiler().withTunable("releaseUnneededInfoset", "false").compileNode(sch)
    assertFalse(pf.getDiagnostics.toString, pf.isError)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor].withExternalVariables(extVars)
    assertFalse(dp.getDiagnostics.toString, dp.isError)

    val inputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx = new UnparseSharedContext(
      inputter.documentElement,
      new VariableBox(dp.variableMap.copy()),
      new SuspensionTracker(
        dp.tunables.unparseSuspensionWaitYoung,
        dp.tunables.unparseSuspensionWaitOld
      ),
      dp,
      dp.tunables,
      prefetchLimit = 100
    )
    val buildState = new BuildState(inputter, sharedCtx, Nil, false)
    buildState.initializeVariables()

    // No write coroutine is constructed/wired into sharedCtx, so build
    // never pauses to interleave with write; the whole point (see class
    // doc): isolates the NVI stale-read bug from the separate, unrelated
    // interleaved-setVariable bug.
    val rootUnparser = dp.ssrd.unparser
    rootUnparser.unparse1(buildState)

    val rootNode = inputter.documentElement.child(0).asComplex
    assertEquals(2, rootNode.numChildren) // the `record` array term, and `summary`
    assertEquals(numRecords, rootNode.child(0).asArray.numChildren)
    val summaryNode = rootNode.child(1).asSimple
    assertEquals("summary", summaryNode.erd.name)
    assertEquals(
      "expected summary to read runningVar's own top-level (externally-bound) " +
        "value, not a stale leftover instance from whichever record build pushed last",
      -1,
      summaryNode.dataValue.getInt
    )
  }
}
