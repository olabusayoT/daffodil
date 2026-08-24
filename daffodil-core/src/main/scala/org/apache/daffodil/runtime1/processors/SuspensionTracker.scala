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

import scala.collection.mutable
import scala.collection.mutable.Queue

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError

class SuspensionTracker(suspensionWaitYoung: Int, suspensionWaitOld: Int) {

  private val suspensionsYoung = new Queue[Suspension]
  private val suspensionsOld = new Queue[Suspension]

  /**
   * Suspensions parked out of the per-young-tick rotation, each with a
   * targeted wake-up already registered against whatever it's blocked on.
   * Given a real retry only on the same reduced cadence as old suspensions
   * (a registered wake-up usually fires first), plus requireFinal's final
   * catch-all.
   */
  private val suspensionsParked = new mutable.HashSet[Suspension]

  /**
   * Every still-tracked, not-yet-done suspension, for debugging use
   * only: combining three buckets into one Seq allocates and copies, so
   * nothing in the unparse hot path should call this. Must include
   * suspensionsParked, or a parked suspension looks like resolved progress.
   */
  def suspensions: Seq[Suspension] =
    suspensionsYoung.toSeq ++ suspensionsOld.toSeq ++ suspensionsParked.toSeq

  private var count: Int = 0

  private var suspensionStatTracked: Int = 0
  private var suspensionStatRuns: Int = 0

  def trackSuspension(s: Suspension): Unit = {
    suspensionsYoung.enqueue(s)
    suspensionStatTracked += 1
  }

  /**
   * Attempts to evaluate suspensions. Old suspensions are evaluated less
   * frequently than young suspensions. A suspension with isWaitingOnWaiter
   * true has a targeted wake-up already registered, so it's parked here
   * instead of wasting a retry on it.
   */
  def evalSuspensions(): Unit = evalSuspensionsThrottled()

  private def evalSuspensionsThrottled(): Unit = {
    if (count % suspensionWaitOld == 0) {
      evalSuspensionQueue(suspensionsOld, skipWaiters = true)
      evalParkedSuspensions()
    }
    if (count % suspensionWaitYoung == 0) {
      evalSuspensionQueue(suspensionsYoung, skipWaiters = true)
      foldYoungIntoOld()
    }

    if (count == suspensionWaitOld) {
      count = 0
    } else {
      count += 1
    }
  }

  // Some suspensions only ever resolve through a real, unconditional
  // retry rather than their own registered wake-up actually firing (a
  // length that only becomes computable through the DOS-splitting
  // machinery's cumulative progress, not one identifiable event). This
  // bounds how long such a suspension waits to requireFinal.
  private def evalParkedSuspensions(): Unit = {
    if (suspensionsParked.isEmpty) return
    val toRetry = new Queue[Suspension]
    toRetry ++= suspensionsParked
    suspensionsParked.clear()
    evalSuspensionQueue(toRetry)
    // Re-park only what's still waiting on a registered waiter; anything
    // else left over blocked on something unrelated, so it belongs back
    // in the normal rotation to get real retries again, not skip-parked
    // forever.
    toRetry.foreach { s =>
      if (s.isWaitingOnWaiter) suspensionsParked.add(s) else suspensionsOld.enqueue(s)
    }
  }

  private def foldParkedIntoOld(): Unit = {
    suspensionsParked.foreach(suspensionsOld.enqueue(_))
    suspensionsParked.clear()
  }

  private def foldYoungIntoOld(): Unit = {
    while (suspensionsYoung.nonEmpty) {
      suspensionsOld.enqueue(suspensionsYoung.dequeue())
    }
  }

  /**
   * Called once a suspension's targeted wake-up actually fires. s may not
   * be parked yet (still in young/old) - then there's nothing to move,
   * since clearing isWaitingOnWaiter already keeps it from being
   * re-parked next time it's visited.
   */
  def moveParkedToYoung(s: Suspension): Unit = {
    if (suspensionsParked.remove(s)) {
      suspensionsYoung.enqueue(s)
    }
  }

  /**
   * Evaluates all suspensions until deadlocked, folding parked and young
   * into old for one final unconditional attempt each - other unrelated
   * preconditions may have resolved by now even if this suspension's own
   * registered wake-up never fired. Still-stuck ones are reported deadlocked.
   */
  def requireFinal(): Unit = {
    foldParkedIntoOld()
    foldYoungIntoOld()

    evalSuspensionQueue(suspensionsOld)

    Assert.invariant(
      suspensionsOld.length != 1,
      "Single suspended expression making no forward progress. " + suspensionsOld(0)
    )

    if (suspensionsOld.nonEmpty) {
      throw new SuspensionDeadlockException(suspensionsOld.toSeq)
    }

    Logger.log.debug(
      f"Suspension runs/tracked: ${suspensionStatRuns}%d/${suspensionStatTracked}%d (${(suspensionStatRuns.toFloat / suspensionStatTracked) * 100}%.2f%%)"
    )
  }

  /**
   * Repeatedly attempts suspensions on queue until no progress is made;
   * still-blocked ones go back on the queue. A suspension with
   * isWaitingOnWaiter true is parked instead of really attempted: a
   * guaranteed external wake-up already exists for it.
   */
  private def evalSuspensionQueue(
    queue: Queue[Suspension],
    skipWaiters: Boolean = false
  ): Unit = {
    var countOfNotMakingProgress = 0
    while (!queue.isEmpty && countOfNotMakingProgress < queue.length) {
      val s = queue.dequeue()
      if (s.isDone) {
        // resolved out of band; queue got smaller for free, so this
        // counts as progress the same as a successful run below.
        countOfNotMakingProgress = 0
      } else if (skipWaiters && s.isWaitingOnWaiter) {
        // Guaranteed external wake-up exists (see the doc comment above):
        // park it, removing it from the rotation entirely. Same
        // dequeue-without-re-enqueue shrink as the isDone branch, so it
        // resets the counter the same way.
        suspensionsParked.add(s)
        countOfNotMakingProgress = 0
      } else {
        suspensionStatRuns += 1
        s.runSuspension()
        if (!s.isDone) queue.enqueue(s)
        if (s.isDone || s.isMakingProgress) {
          countOfNotMakingProgress = 0
        } else {
          countOfNotMakingProgress += 1
        }
      }
    }
  }

}

class SuspensionDeadlockException(suspExprs: Seq[Suspension])
  extends RuntimeSchemaDefinitionError(
    suspExprs(0).rd.schemaFileLocation,
    "Expressions/Unparsers are circularly deadlocked (mutually defined):\n%s",
    suspExprs
      .groupBy {
        _.rd
      }
      .view
      .mapValues {
        _(0)
      }
      .values
      .mkString(" - ", "\n - ", "")
  )
