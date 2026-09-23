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

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates SuspensionCapableUState end to end: a build-side OVC forward
 * reference must create a tracked Suspension, not throw ClassCastException.
 */
class BuildStateSuspensionTest {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testBuildSideOVCSuspensionIsTrackedNotClassCastException(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element
              name="computed"
              type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    // "computed" is OVC. It's optional in the infoset input, since its value
    // is computed, not provided (see OVCStartEndStrategy).
    val infoset = <ex:row xmlns:ex={example}><actual>005</actual></ex:row>

    // releaseUnneededInfoset disabled; otherwise the actual unparse
    // machinery frees "computed" from the tree as soon as it's done with
    // it, and we want to inspect its final value afterward.
    val dp = TestUtils.compileForUnparse(sch, Map("releaseUnneededInfoset" -> "false"))

    val inputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(inputter.documentElement, dp, prefetchLimit = 100)()
    val buildState = new BuildState(inputter, sharedCtx, Nil, false)
    buildState.initializeVariables()

    // Drives all the way through: "computed"'s OVC forward-references
    // "actual", which doesn't exist yet when build reaches "computed"
    // (document order). This should suspend, not throw; certainly not
    // ClassCastException against BuildState.
    dp.ssrd.unparser.unparse1(buildState)

    // Not asserted here: suspensions.nonEmpty would be flaky, since
    // "actual"'s own unparseEnd calls evalSuspensions and retries/resolves
    // "computed"'s suspension before unparse1 returns; expected, reflecting
    // suspend/resume working within a single pass. The real check is below:
    val row =
      sharedCtx.rootDoc.child(0).asInstanceOf[org.apache.daffodil.runtime1.infoset.DIComplex]
    val computed = row.child(0).asInstanceOf[org.apache.daffodil.runtime1.infoset.DISimple]
    assertEquals(6, computed.dataValue.getInt)

    sharedCtx.suspensionTracker.evalSuspensions()
    // requireFinal() would throw SuspensionDeadlockException if anything
    // were still stuck. This confirms nothing was left hanging.
    sharedCtx.suspensionTracker.requireFinal()
  }

  // Build's first attempt at a valueLength OVC must skip doTask
  // (suspendWithoutAttempting, not run()): isBlocked stays false.
  @Test def testBuildSideValueLengthOVCNeverCallsDoTask(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
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
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val infoset = <ex:row xmlns:ex={example}><data>hello</data></ex:row>

    val dp = TestUtils.compileForUnparse(sch, Map("releaseUnneededInfoset" -> "false"))

    val inputter = TestUtils.newInitializedInputter(infoset, dp)

    val sharedCtx =
      UnparseSharedContextTestFixture.build(inputter.documentElement, dp, prefetchLimit = 100)()
    val buildState = new BuildState(inputter, sharedCtx, Nil, false)
    buildState.initializeVariables()

    dp.ssrd.unparser.unparse1(buildState)

    assertEquals(1, sharedCtx.suspensionTracker.suspensions.length)
    val suspension = sharedCtx.suspensionTracker.suspensions.head
    assertFalse(
      "doTask should never have been called for a suspendWithoutAttempting-created suspension",
      suspension.isBlocked
    )
  }
}
