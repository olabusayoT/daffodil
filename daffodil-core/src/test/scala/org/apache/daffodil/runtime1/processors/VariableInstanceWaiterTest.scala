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

package org.apache.daffodil.runtime1.processors

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.infoset.DataValue
import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard for VariableInstance's suspensionWaiter and the
 * VariableMap operations that must wake registrants a plain notify can't
 * reach: a suspension blocked reading a variable must be woken once
 * setValue actually sets it, and newVariableInstance/removeVariableInstance
 * must force-retry every registrant of the instance they're
 * shadowing/unshadowing, since nothing will ever notify that instance
 * again once it's no longer the live one.
 */
class VariableInstanceWaiterTest {

  private def compiledVariableMap(): VariableMap = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>
        ++ <dfdl:defineVariable name="v" type="xs:int"/>,
      <xs:element name="root" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>,
      elementFormDefault = "unqualified"
    )
    val dp = TestUtils.compileSchema(sch)
    dp.variableMap
  }

  private def newSuspension(vrd: VariableRuntimeData): Suspension = new Suspension {
    override def rd = vrd
    override protected def doTask(ustate: UState): Unit = ()
  }

  @Test def testSetValueNotifiesSuspensionWaiter(): Unit = {
    val vmap = compiledVariableMap()
    val vrd = vmap.vrds.find(_.globalQName.local == "v").get
    val inst = vmap.find(vrd.globalQName).get

    val suspension = newSuspension(vrd)
    suspension.setDone()
    suspension.registerWaiter(inst.suspensionWaiter)
    assertFalse(inst.suspensionWaiter.isEmpty)

    inst.setValue(DataValue.unsafeFromAnyRef(java.lang.Integer.valueOf(1)))
    assertTrue(
      "expected setValue to notify (and thus clear) the suspensionWaiter " +
        "once the variable's value became defined",
      inst.suspensionWaiter.isEmpty
    )
  }

  @Test def testConditionGatesNotify(): Unit = {
    val vmap = compiledVariableMap()
    val vrd = vmap.vrds.find(_.globalQName.local == "v").get
    val inst = vmap.find(vrd.globalQName).get

    val suspension = newSuspension(vrd)
    // Counts condition invocations rather than just its (always-false)
    // result, so this test can tell "notified, then re-checked and
    // stayed false" apart from "never notified at all".
    var condChecked = 0
    suspension.registerWaiter(inst.suspensionWaiter, () => { condChecked += 1; false })
    assertFalse(inst.suspensionWaiter.isEmpty)

    inst.setValue(DataValue.unsafeFromAnyRef(java.lang.Integer.valueOf(1)))

    assertEquals(
      "expected setValue's notify to re-verify the condition exactly once",
      1,
      condChecked
    )
    assertFalse(
      "expected the suspension to remain registered: its condition never holds",
      inst.suspensionWaiter.isEmpty
    )
    assertTrue("expected isParked to remain true", suspension.isParked)
  }

  @Test def testNewVariableInstanceForceRetriesShadowedInstance(): Unit = {
    val vmap = compiledVariableMap()
    val vrd = vmap.vrds.find(_.globalQName.local == "v").get
    val shadowed = vmap.find(vrd.globalQName).get

    val suspension = newSuspension(vrd)
    // done_ becomes true before registerWaiter, so forceRetryAll's own
    // !isDone guard skips moveFromParkedToYoung, which a bare test-double
    // suspension (never actually suspended against a real UState) can't
    // survive.
    suspension.setDone()
    suspension.registerWaiter(shadowed.suspensionWaiter, () => false)
    assertFalse(shadowed.suspensionWaiter.isEmpty)

    vmap.newVariableInstance(vrd)

    assertTrue(
      "expected newVariableInstance to force-retry (and thus clear) every " +
        "registrant of the instance it shadows",
      shadowed.suspensionWaiter.isEmpty
    )
  }

  @Test def testRemoveVariableInstanceForceRetriesUnshadowedInstance(): Unit = {
    val vmap = compiledVariableMap()
    val vrd = vmap.vrds.find(_.globalQName.local == "v").get
    val shadowing = vmap.newVariableInstance(vrd)

    val suspension = newSuspension(vrd)
    // Same reasoning as testNewVariableInstanceForceRetriesShadowedInstance:
    // a bare test-double suspension can't survive a real
    // moveFromParkedToYoung hand-off.
    suspension.setDone()
    suspension.registerWaiter(shadowing.suspensionWaiter, () => false)
    assertFalse(shadowing.suspensionWaiter.isEmpty)

    vmap.removeVariableInstance(vrd)

    assertTrue(
      "expected removeVariableInstance to force-retry (and thus clear) " +
        "every registrant of the instance it unshadows",
      shadowing.suspensionWaiter.isEmpty
    )
  }
}
