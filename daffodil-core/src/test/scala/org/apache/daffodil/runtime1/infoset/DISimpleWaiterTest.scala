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

package org.apache.daffodil.runtime1.infoset

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.processors.DataProcessor

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard for DISimple's suspensionWaiter: unparse operations
 * that block on hasValue/isNilled must be woken once those become true,
 * not just resolved eventually by the periodic fallback retry.
 *
 * Registers directly against the waiter and marks the suspension done
 * beforehand, exercising the notify path itself without the heavier
 * real-unparse machinery a live suspend/retry cycle would require.
 */
class DISimpleWaiterTest {

  private def compileTrivialSchema(): DataProcessor = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="root" nillable="true" type="xs:string"
        dfdl:nilKind="literalCharacter" dfdl:nilValueDelimiterPolicy="none"
        dfdl:nilValue="%NUL;" dfdl:lengthKind="explicit" dfdl:length="1"/>,
      elementFormDefault = "unqualified"
    )
    TestUtils.compileSchema(sch)
  }

  @Test def testSetDataValueNotifiesSuspensionWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val elem = new DISimple(dp.ssrd.elementRuntimeData)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(elem.erd)
    suspension.registerWaiter(elem.suspensionWaiter)
    assertFalse(elem.suspensionWaiter.isEmpty)

    assertFalse(elem.hasValue)
    elem.setDataValue("x")
    assertTrue(elem.hasValue)
    assertTrue(
      "expected setDataValue to notify (and thus clear) the suspensionWaiter " +
        "once hasValue became true",
      elem.suspensionWaiter.isEmpty
    )
  }

  @Test def testSetNilledNotifiesSuspensionWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val elem = new DISimple(dp.ssrd.elementRuntimeData)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(elem.erd)
    suspension.registerWaiter(elem.suspensionWaiter)
    assertFalse(elem.suspensionWaiter.isEmpty)

    assertFalse(elem.isNilled)
    elem.setNilled()
    assertTrue(elem.isNilled)
    assertTrue(
      "expected setNilled to notify (and thus clear) the suspensionWaiter " +
        "once isNilled became true",
      elem.suspensionWaiter.isEmpty
    )
  }

  /**
   * Regression guard for SuspensionWaiter's per-registration condition:
   * a not-done suspension whose condition doesn't hold yet must stay
   * registered through a real setNilled-triggered notify, not be dropped
   * just because some unrelated fact on the same node settled.
   */
  @Test def testConditionGatesNotify(): Unit = {
    val dp = compileTrivialSchema()
    val elem = new DISimple(dp.ssrd.elementRuntimeData)

    val suspension = SuspensionWaiterTestUtils.newSuspension(elem.erd)
    // Counts condition invocations rather than just its (always-false)
    // result, so this test can tell "notified, then re-checked and
    // stayed false" apart from "never notified at all".
    var condChecked = 0
    suspension.registerWaiter(elem.suspensionWaiter, () => { condChecked += 1; elem.hasValue })
    assertFalse(elem.suspensionWaiter.isEmpty)

    elem.setNilled()

    assertEquals(
      "expected setNilled's notify to re-verify the condition exactly once",
      1,
      condChecked
    )
    assertFalse(
      "expected the suspension to remain registered: setNilled's notify " +
        "fired, but hasValue still doesn't hold",
      elem.suspensionWaiter.isEmpty
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }
}
