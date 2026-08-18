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

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.junit.Assert.*
import org.junit.Test

/**
 * Regression guard for Suspension.registerLengthStateWaiter /
 * LengthState.removeWaiter: a suspension whose length dependency shifts
 * from element A to element B between retries must be deregistered from
 * A's waiters list once it registers with B's - otherwise A's eventual
 * notifyWaiters() would retry it pointlessly even though it now depends
 * on B, not A.
 *
 * Drives LengthState/Suspension directly with a minimal Suspension double
 * (registerWaiter/removeWaiter only compare by reference identity, never
 * call any method on it) rather than through a full DFDL schema: reliably
 * forcing a real dependency shift via schema+tunable timing isn't
 * deterministic, while this exercises the exact bookkeeping that changed.
 */
class LengthStateWaiterTest {

  private def compileTrivialSchema(): DataProcessor = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val pf = Compiler().compileNode(sch)
    assertFalse(pf.getDiagnostics.toString, pf.isError)
    pf.onPath("/").asInstanceOf[DataProcessor]
  }

  @Test def testDeregistersFromPriorLengthStateOnShift(): Unit = {
    val dp = compileTrivialSchema()

    // Two distinct DIElement instances is all this needs - the
    // register/remove bookkeeping never inspects element content, so
    // there's no need to drive a real parse/unparse to populate one.
    val elementA = new DISimple(dp.ssrd.elementRuntimeData)
    val elementB = new DISimple(dp.ssrd.elementRuntimeData)

    val lengthStateA = new ContentLengthState(elementA)
    val lengthStateB = new ContentLengthState(elementB)

    // registerWaiter/removeWaiter only ever compare by reference identity;
    // neither doTask nor rd is ever actually invoked by the code under
    // test here.
    val suspension = new Suspension {
      override def rd = elementA.erd
      override protected def doTask(ustate: UState): Unit = ()
    }

    suspension.registerLengthStateWaiter(lengthStateA)
    assertTrue(
      "expected the suspension to be registered on A right after registering",
      lengthStateA.isRegisteredWaiter(suspension)
    )

    // The suspension's dependency shifts to B on a later retry, without A
    // ever having resolved (A.notifyWaiters was never called).
    suspension.registerLengthStateWaiter(lengthStateB)

    assertFalse(
      "expected the suspension to be deregistered from A once it registered with B instead",
      lengthStateA.isRegisteredWaiter(suspension)
    )
    assertTrue(
      "expected the suspension to be registered on B",
      lengthStateB.isRegisteredWaiter(suspension)
    )

    // Re-registering with the SAME LengthState it's already on must stay
    // a no-op removal/re-add (dedup by reference identity), not spuriously
    // drop it.
    suspension.registerLengthStateWaiter(lengthStateB)
    assertTrue(
      "expected re-registering with the same LengthState to remain a no-op",
      lengthStateB.isRegisteredWaiter(suspension)
    )
  }
}
