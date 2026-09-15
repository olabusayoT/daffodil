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

import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.junit.Assert.*
import org.junit.Test

class TestSuspensionTracker {

  private def newSuspension(): Suspension = new Suspension {
    override def rd = throw new NotImplementedError("not used by this test")
    override val isReadOnly = true
    override protected def doTask(ustate: UState): Unit = ()
  }

  /**
   * A parked suspension resolved out-of-band (neither its own registered
   * wake-up nor evalParkedSuspensions' own retry) must be pruned on the
   * next sweep instead of being fed back into evalSuspensionQueue, which
   * asserts a dequeued suspension is never already done.
   */
  @Test def testResolvedParkedSuspensionGetsPruned(): Unit = {
    val tracker = new SuspensionTracker(suspensionWaitYoung = 1, suspensionWaitOld = 1)
    val s = newSuspension()
    tracker.trackSuspension(s)
    // Simulates a targeted wake-up already registered (e.g. DPath.scala's
    // InfosetLengthUnknownException catch clause) by the time the first
    // sweep dequeues this suspension.
    s.registerWaiter(new SuspensionWaiter)

    // First tick (wait=1 makes every call sweep unconditionally): sweeps
    // suspensionsYoung, finds isParked, parks it.
    tracker.evalSuspensions()
    assertTrue(tracker.suspensions.exists(_ eq s))

    // Resolve it out of band, like a genuine targeted wake-up would -
    // directly, independent of which bucket it currently sits in.
    s.setDone()
    assertTrue(s.isDone)

    // Next periodic tick must prune it rather than re-attempting it.
    tracker.evalSuspensions()
    assertFalse(tracker.suspensions.exists(_ eq s))

    tracker.requireFinal()
  }
}
