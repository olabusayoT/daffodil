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

import org.junit.Assert.*
import org.junit.Test

/**
 * Exercises BuildCoroutine/WriteCoroutine directly (no schema, no DIElement)
 * to verify the prefetch-window/coroutine-handoff mechanism itself: build
 * never runs more than windowNodes+1 elements ahead of write, and control
 * correctly ping-pongs back and forth until build finishes.
 */
class BuildWritePrefetchCoroutineTest {

  /**
   * Drives a BuildCoroutine whose build work just calls elementBuilt()
   * totalElements times, pausing each time it hands control back to let
   * write "catch up" by draining drainPerTurn elements first.
   */
  private def runToCompletion(
    windowNodes: Int,
    totalElements: Int,
    drainPerTurn: Int
  ): BuildCoroutine = {
    val writeCoroutine = new WriteCoroutine
    var buildCoroutine: BuildCoroutine = null
    buildCoroutine = new BuildCoroutine(
      writeCoroutine,
      windowNodes,
      () => {
        var i = 0
        while (i < totalElements) {
          buildCoroutine.elementBuilt()
          i += 1
        }
      }
    )

    var progress: BuildProgress = writeCoroutine.resume(buildCoroutine, ())
    var iterations = 0
    while (progress == BuildProgress.WindowFilled) {
      iterations += 1
      assertTrue("test should not spin forever", iterations < 10 * totalElements + 100)
      var drained = 0
      while (drained < drainPerTurn) {
        buildCoroutine.elementWritten()
        drained += 1
      }
      progress = writeCoroutine.resume(buildCoroutine, ())
    }
    assertEquals(BuildProgress.Finished, progress)
    buildCoroutine
  }

  @Test def testBuildNeverExceedsWindowByMoreThanOne(): Unit = {
    val windowNodes = 5
    val buildCoroutine = runToCompletion(windowNodes, totalElements = 200, drainPerTurn = 1)
    // Build pauses the instant its lead *exceeds* the window, so the
    // largest lead it can ever reach is windowNodes + 1.
    assertEquals(windowNodes + 1, buildCoroutine.maxLeadObserved)
  }

  @Test def testWindowActuallyBoundsHowFarAheadBuildRuns(): Unit = {
    // With write draining only one element per turn, a small document
    // (fewer elements than the window) never fills the window at all: build
    // races to completion in a single uninterrupted turn.
    val buildCoroutine =
      runToCompletion(windowNodes = 1000, totalElements = 10, drainPerTurn = 1)
    assertEquals(10, buildCoroutine.maxLeadObserved)
  }

  @Test def testZeroWindowStillMakesProgressOneElementAtATime(): Unit = {
    val buildCoroutine = runToCompletion(windowNodes = 0, totalElements = 50, drainPerTurn = 1)
    assertEquals(1, buildCoroutine.maxLeadObserved)
  }

  @Test def testWriteDrainingMultipleElementsPerTurnStillRespectsWindow(): Unit = {
    val buildCoroutine = runToCompletion(windowNodes = 4, totalElements = 97, drainPerTurn = 3)
    assertEquals(5, buildCoroutine.maxLeadObserved)
  }

  @Test def testBuildFailurePropagatesAsFailedProgress(): Unit = {
    val writeCoroutine = new WriteCoroutine
    val boom = new RuntimeException("build blew up")
    val buildCoroutine = new BuildCoroutine(
      writeCoroutine,
      windowNodes = 10,
      () => throw boom
    )
    val progress = writeCoroutine.resume(buildCoroutine, ())
    progress match {
      case BuildProgress.Failed(e) => assertSame(boom, e)
      case other => fail(s"expected Failed, got $other")
    }
  }
}
