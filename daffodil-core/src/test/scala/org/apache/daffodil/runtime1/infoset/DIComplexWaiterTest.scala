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
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard for DIComplex/DIArray's inherited suspensionWaiter: a
 * suspension blocked on a complex or array gaining a child must be woken
 * once one is added, not just resolved eventually by the periodic
 * fallback retry.
 *
 * Registers directly against the waiter and marks the suspension done
 * beforehand, exercising the notify path itself without the heavier
 * real-unparse machinery a live suspend/retry cycle would require.
 */
class DIComplexWaiterTest {

  private def compileTrivialSchema(): DataProcessor = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    TestUtils.compileSchema(sch)
  }

  private def compileArraySchema(): DataProcessor = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="item" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"
              minOccurs="0" maxOccurs="unbounded" dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    TestUtils.compileSchema(sch)
  }

  @Test def testSetFinalNotifiesSuspensionWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val diComplex = new DIComplex(rootErd)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(rootErd)
    suspension.registerWaiter(diComplex.suspensionWaiter)
    assertFalse(diComplex.suspensionWaiter.isEmpty)

    diComplex.setFinal()

    assertTrue(
      "expected setFinal to notify (and thus clear) the suspensionWaiter",
      diComplex.suspensionWaiter.isEmpty
    )
  }

  @Test def testAddChildNotifiesSuspensionWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val diComplex = new DIComplex(rootErd)
    val child = new DISimple(rootErd)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(rootErd)
    suspension.registerWaiter(diComplex.suspensionWaiter)
    assertFalse(diComplex.suspensionWaiter.isEmpty)

    diComplex.addChild(child, DaffodilTunables())

    assertTrue(
      "expected addChild to notify (and thus clear) the suspensionWaiter " +
        "once a new named child was added",
      diComplex.suspensionWaiter.isEmpty
    )
  }

  /**
   * Regression guard for DIComplex.addChild's needsNewArray branch: the
   * notify that fires when a repeating child's first occurrence creates
   * its DIArray, distinct from DIArray.append's own notify once that
   * array already exists (covered by testArrayAppendNotifiesSuspensionWaiter
   * below).
   */
  @Test def testAddChildNotifiesOnFirstArrayOccurrence(): Unit = {
    val dp = compileArraySchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val itemErd = rootErd.childERDs.find(_.name == "item").get
    val diComplex = new DIComplex(rootErd)
    val firstItem = new DISimple(itemErd)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(rootErd)
    suspension.registerWaiter(diComplex.suspensionWaiter)
    assertFalse(diComplex.suspensionWaiter.isEmpty)

    diComplex.addChild(firstItem, DaffodilTunables())

    assertTrue(
      "expected addChild's new-array branch to notify (and thus clear) " +
        "the suspensionWaiter for the first occurrence of a repeating child",
      diComplex.suspensionWaiter.isEmpty
    )
  }

  @Test def testArrayAppendNotifiesSuspensionWaiter(): Unit = {
    val dp = compileTrivialSchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val diComplexParent = new DIComplex(rootErd)
    val diArray = new DIArray(rootErd, diComplexParent, 1)

    val suspension = SuspensionWaiterTestUtils.newDoneSuspension(rootErd)
    suspension.registerWaiter(diArray.suspensionWaiter)
    assertFalse(diArray.suspensionWaiter.isEmpty)

    diArray.append(new DISimple(rootErd))

    assertTrue(
      "expected append to notify (and thus clear) the suspensionWaiter, " +
        "rather than waiting until setFinal",
      diArray.suspensionWaiter.isEmpty
    )
  }

  @Test def testArrayAppendDoesNotNotifyWhenIndexConditionStillFalse(): Unit = {
    val dp = compileTrivialSchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val diComplexParent = new DIComplex(rootErd)
    val diArray = new DIArray(rootErd, diComplexParent, 1)

    val suspension = SuspensionWaiterTestUtils.newSuspension(rootErd)
    // Counts condition invocations rather than just its (always-false)
    // result, so this test can tell "notified, then re-checked and
    // stayed false" apart from "never notified at all".
    var condChecked = 0
    suspension.registerWaiter(
      diArray.suspensionWaiter,
      () => { condChecked += 1; diArray.length >= 2 }
    )
    assertFalse(diArray.suspensionWaiter.isEmpty)

    diArray.append(new DISimple(rootErd))

    assertEquals(
      "expected append's notify to re-verify the condition exactly once",
      1,
      condChecked
    )
    assertFalse(
      "expected the suspension to remain registered: only 1 of the 2 " +
        "awaited elements has been appended",
      diArray.suspensionWaiter.isEmpty
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }

  /**
   * Regression guard for SuspensionWaiter's per-registration condition:
   * a not-done suspension whose condition doesn't hold yet must stay
   * registered through addChild's notify, not be dropped just because
   * the complex gained an unrelated child.
   */
  @Test def testConditionGatesNotify(): Unit = {
    val dp = compileTrivialSchema()
    val rootErd = dp.ssrd.elementRuntimeData
    val diComplex = new DIComplex(rootErd)
    val child = new DISimple(rootErd)

    val suspension = SuspensionWaiterTestUtils.newSuspension(rootErd)
    // Counts condition invocations rather than just its (always-false)
    // result, so this test can tell "notified, then re-checked and
    // stayed false" apart from "never notified at all".
    var condChecked = 0
    suspension.registerWaiter(diComplex.suspensionWaiter, () => { condChecked += 1; false })
    assertFalse(diComplex.suspensionWaiter.isEmpty)

    diComplex.addChild(child, DaffodilTunables())

    assertEquals(
      "expected addChild's notify to re-verify the condition exactly once",
      1,
      condChecked
    )
    assertFalse(
      "expected the suspension to remain registered: its condition never holds",
      diComplex.suspensionWaiter.isEmpty
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }
}

/**
 * Bare-Suspension test double for this class and DISimpleWaiterTest:
 * registers directly against a suspensionWaiter to exercise the notify
 * path itself, without the heavier real-unparse machinery a live
 * suspend/retry cycle would require.
 */
private[infoset] object SuspensionWaiterTestUtils {

  def newSuspension(forRd: ElementRuntimeData): Suspension = new Suspension {
    override def rd = forRd
    override protected def doTask(ustate: UState): Unit = ()
  }

  // done_ becomes true before any registration, so notifySuspensions'
  // isDone guard skips moveFromParkedToYoung.
  def newDoneSuspension(forRd: ElementRuntimeData): Suspension = {
    val suspension = newSuspension(forRd)
    suspension.setDone()
    suspension
  }
}
