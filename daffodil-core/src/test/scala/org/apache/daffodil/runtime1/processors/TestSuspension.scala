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
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.processors.unparsers.BuildState
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UnparseSharedContext
import org.apache.daffodil.runtime1.processors.unparsers.UnparseSharedContextTestFixture

import org.junit.Assert.*
import org.junit.Test

/**
 * A minimal Suspension whose doTask counts invocations and either
 * blocks or succeeds, under the test's control.
 */
private class SpySuspension(val rd: RuntimeData) extends Suspension {

  override val isReadOnly = true

  // Avoids prepareToSuspend's splitDOS branch (requires an actual
  // currentInfosetNode to be positioned); irrelevant here since this test
  // drives the suspension directly, not through an actual unparse traversal.
  override protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong(0L)

  var doTaskCallCount: Int = 0
  private var shouldSucceed = false

  def markShouldSucceedNextTime(): Unit = shouldSucceed = true

  protected def doTask(ustate: UState): Unit = {
    doTaskCallCount += 1
    if (shouldSucceed) {
      setDone()
    } else {
      block(this, this, 0, this)
    }
  }
}

/**
 * Unit tests for suspendWithoutAttempting: confirms it tracks/suspends
 * like a genuine blocked run(), but without ever calling doTask.
 */
class TestSuspension {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  /** A trivial schema/BuildState, just to get an actual UState to drive the
    * spy against; suspensionWaitYoung/Old default to schema-compiled tunables,
    * or pass explicit values (e.g. 1) for a deterministic sweep cadence.
    */
  private def newBuildState(
    suspensionWaitYoung: Int = -1,
    suspensionWaitOld: Int = -1
  ): (DataProcessor, BuildState, UnparseSharedContext) = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="row" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}>123</ex:row>

    val dp = TestUtils.compileForUnparse(sch)

    val inputter = TestUtils.newInitializedInputter(infoset, dp)

    // Doubling only applies to the tunable-derived fallback, matching
    // production's build/write-interleaving compensation (see
    // UnparseSharedContextTestFixture); an explicit override (e.g. 1, to
    // force every call to sweep unconditionally) is used as given.
    val waitYoung = if (suspensionWaitYoung > 0) {
      suspensionWaitYoung
    } else {
      dp.tunables.unparseSuspensionWaitYoung * 2
    }
    val waitOld = if (suspensionWaitOld > 0) {
      suspensionWaitOld
    } else {
      dp.tunables.unparseSuspensionWaitOld * 2
    }
    val sharedCtx = UnparseSharedContextTestFixture.build(
      inputter.documentElement,
      dp,
      prefetchLimit = 100
    )(waitYoung, waitOld)
    val buildState = new BuildState(inputter, sharedCtx, Nil, false)
    buildState.initializeVariables()
    // cloneForSuspension reads currentInfosetNodeStack.top, normally
    // pushed at the start of a real element unparse; since this test
    // drives the suspension directly rather than through a real unparse
    // traversal, push a minimal entry manually (the other two stacks
    // auto-push already).
    buildState.currentInfosetNodeStack.push(One(sharedCtx.rootDoc))
    // Likewise normally set at the start of a real element unparse;
    // needed by cloneForSuspension's own processor-copying call.
    buildState.setProcessor(dp.ssrd.unparser)
    (dp, buildState, sharedCtx)
  }

  @Test def testSuspendWithoutAttemptingNeverCallsDoTask(): Unit = {
    val (dp, buildState, sharedCtx) = newBuildState()
    val spy = new SpySuspension(dp.ssrd.elementRuntimeData)

    spy.suspendWithoutAttempting(buildState)

    assertEquals(0, spy.doTaskCallCount)
    assertFalse(spy.isDone)
  }

  @Test def testSuspendWithoutAttemptingIsTrackedAndLaterResolves(): Unit = {
    val (dp, buildState, sharedCtx) = newBuildState()
    val spy = new SpySuspension(dp.ssrd.elementRuntimeData)

    spy.suspendWithoutAttempting(buildState)
    assertTrue(sharedCtx.suspensionTracker.suspensions.exists(_ eq spy))

    // Now make a genuine attempt succeed, and confirm the tracker's unfiltered
    // drain resolves it; suspendWithoutAttempting only skips the wasted
    // first attempt, not the suspension's normal later resolution.
    spy.markShouldSucceedNextTime()
    sharedCtx.suspensionTracker.evalSuspensionsUnthrottled()

    assertEquals(1, spy.doTaskCallCount)
    assertTrue(spy.isDone)
    sharedCtx.suspensionTracker.requireFinal()
  }
}
