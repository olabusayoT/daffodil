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

import scala.collection.mutable.Queue

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.runtime1.dsom.RuntimeSchemaDefinitionError

class SuspensionTracker(suspensionWaitYoung: Int, suspensionWaitOld: Int) {

  private val suspensionsYoung = new Queue[Suspension]
  private val suspensionsOld = new Queue[Suspension]

  /**
   * Suspensions unable to make progress until something external
   * changes (a targeted wake-up firing, or real bytes getting written);
   * moved out of suspensionsYoung/suspensionsOld so the periodic sweep
   * (evalSuspensionsThrottled) doesn't keep re-visiting them every tick -
   * even a cheap per-item skip costs real time at scale.
   *
   * Only drained by foldParkedIntoOld, from the two "must attempt
   * everything" methods below (evalSuspensionsUnthrottled, requireFinal);
   * the targeted wake-up itself (LengthState.notifyWaiters) retries a
   * suspension directly regardless of which bucket it's in, so parking
   * never delays that path.
   */
  private val suspensionsParked = new Queue[Suspension]

  /**
   * Every still-tracked, not-yet-done suspension across all buckets.
   * Must include suspensionsParked, or a suspension moving into that
   * bucket would look like (incorrectly) resolved progress to a caller
   * comparing this count before/after evalSuspensionsUnthrottled().
   */
  def suspensions: Seq[Suspension] =
    suspensionsYoung.toSeq ++ suspensionsOld.toSeq ++ suspensionsParked.toSeq

  /**
   * Count of suspensions currently parked (suspensionsParked above) -
   * unable to progress until the real bytes their wake-up depends on
   * actually get written (i.e. by a non-discard-sink sweep). Not a good
   * backlog-sized throttle on its own (see pendingCount below): a
   * suspension is only classified here once it's actually re-retried
   * and re-blocks on InfosetLengthUnknownException. A discard-sink sweep
   * (evalBuildResolvableSuspensions) never performs that retry - it
   * always skip-and-requeues instead - so nothing parks purely from a
   * discard-sink sweep before a real sweep (evalSuspensions) has run at
   * least once.
   */
  def parkedCount: Int = suspensionsParked.length

  /**
   * Total not-yet-done suspensions across all three buckets, without
   * allocating (unlike `suspensions` above - not safe to call once per
   * node). Unlike parkedCount, grows the moment a suspension is created
   * (trackSuspension), with no dependency on it having been retried yet
   * - intended as a throttle signal for pacing a discard-sink traversal
   * against the pending backlog. Still distinguishes the two workload
   * shapes correctly: a canResolveWithoutWriting=true suspension
   * resolves within a few ticks of its sibling being added to the tree
   * (stays small/transient), while a length-dependent one (never
   * resolvable without real bytes actually being written) accumulates
   * here unboundedly.
   */
  def pendingCount: Int =
    suspensionsYoung.length + suspensionsOld.length + suspensionsParked.length

  private var count: Int = 0

  private var suspensionStatTracked: Int = 0
  private var suspensionStatRuns: Int = 0

  def trackSuspension(s: Suspension): Unit = {
    suspensionsYoung.enqueue(s)
    suspensionStatTracked += 1
  }

  /**
   * Attempts to evaluate suspensions. Old suspensions are evaluated less
   * frequently than young suspensions. Any young suspensions that fail to
   * evaluate are moved to the old suspensions list. If we evaluate old
   * suspensions, we attempt to evaluate them first, with the hope that their
   * resolution might make the young suspensions more likely to evaluate.
   *
   * skipLengthStateWaiters = true here: a suspension with
   * isWaitingOnLengthState true has a targeted wake-up already
   * registered (fired from CaptureEndOf{Content,Value}LengthUnparsers
   * once its length becomes computable) and can't progress until that
   * fires - retrying it on the blind periodic schedule first is pure
   * wasted DPath re-evaluation.
   */
  def evalSuspensions(): Unit =
    evalSuspensionsThrottled(filterToBuildResolvable = false, skipLengthStateWaiters = true)

  /**
   * A discard-sink sweep variant: same throttled cadence as
   * evalSuspensions, but passes filterToBuildResolvable=true to
   * evalSuspensionQueue. A suspension whose canResolveWithoutWriting is
   * false can never be satisfied by a discard-sink traversal no matter
   * how many retries, so it's skipped-and-requeued instead of really
   * attempted - unless it's already isWaitingOnLengthState, in which
   * case it's parked instead (parking only ever follows one of the real
   * sweep's (evalSuspensions) own unfiltered attempts having set that
   * flag; this filtered sweep never sets it itself). Either way the
   * suspension stays pending for that real sweep's later, unfiltered
   * attempts once real bytes exist for it to depend on.
   *
   * Eliminates the wasted doTask cost of this discard-sink sweep for
   * these suspensions; doesn't eliminate the smaller per-tick
   * dequeue/requeue cost for suspensions with no targeted wake-up at all
   * (e.g. padding/target-length SuspendableOperations), which must stay
   * on the skip-and-requeue path so the real sweep still finds them.
   *
   * skipLengthStateWaiters is left false here: canResolveWithoutWriting
   * already excludes every length-state-blocked suspension from this
   * sweep's retries, and more completely (it also covers non-length
   * forward references), so a second filter would be redundant.
   */
  def evalBuildResolvableSuspensions(): Unit =
    evalSuspensionsThrottled(filterToBuildResolvable = true)

  private def evalSuspensionsThrottled(
    filterToBuildResolvable: Boolean,
    skipLengthStateWaiters: Boolean = false
  ): Unit = {
    if (count % suspensionWaitOld == 0) {
      evalSuspensionQueue(suspensionsOld, filterToBuildResolvable, skipLengthStateWaiters)
    }
    if (count % suspensionWaitYoung == 0) {
      evalSuspensionQueue(suspensionsYoung, filterToBuildResolvable, skipLengthStateWaiters)
      while (suspensionsYoung.nonEmpty) {
        suspensionsOld.enqueue(suspensionsYoung.dequeue())
      }
      // suspensionsParked is excluded from the periodic sweep above
      // (the whole point of parking), but that means nothing else
      // removes an entry once it resolves out-of-band via a targeted
      // wake-up - only foldParkedIntoOld does, once at the very end of
      // the document. Without this prune, a resolved-but-parked
      // suspension (and everything it retains) stays reachable for the
      // rest of the document. A plain isDone check is far cheaper than
      // the real doTask attempts this cadence already performs above,
      // so reusing suspensionWaitYoung here is safe and doesn't
      // reintroduce the wasted-re-attempt cost parking avoids.
      suspensionsParked.dequeueAll(_.isDone)
    }

    if (count == suspensionWaitOld) {
      count = 0
    } else {
      count += 1
    }
  }

  /**
   * Attempts every currently-tracked suspension once, bypassing the
   * normal throttling, without treating remaining blocks as an error.
   * Intended for a caller whose own traversal has fully finished and
   * needs a blocked suspension to resolve before it can continue - e.g.
   * a value-only OVC referencing an already-added sibling that just
   * hadn't been retried since that sibling appeared. Suspensions still
   * legitimately blocked (e.g. needing real bytes to be written first)
   * are simply left pending for a later call here or `requireFinal`.
   *
   * Folds suspensionsParked back in first: since this pass is already a
   * one-time, unfiltered sweep of the whole backlog, giving parked
   * entries their first real attempt here is free.
   */
  def evalSuspensionsUnthrottled(): Unit = {
    foldParkedIntoOld()
    evalSuspensionQueue(suspensionsOld)
    evalSuspensionQueue(suspensionsYoung)
    while (suspensionsYoung.nonEmpty) {
      suspensionsOld.enqueue(suspensionsYoung.dequeue())
    }
  }

  private def foldParkedIntoOld(): Unit = {
    while (suspensionsParked.nonEmpty) {
      suspensionsOld.enqueue(suspensionsParked.dequeue())
    }
  }

  /**
   * Evaluates all suspensions until either they are all evaluated or a
   * deadlock is detected. This moves all young suspensions to the old queue,
   * and evaluates all old suspensions. If the old queue is non-empty, that
   * means some suspensions are blocked, likely due to a circular deadlock, and
   * we output diagnostics.
   */
  def requireFinal(): Unit = {
    foldParkedIntoOld()
    while (suspensionsYoung.nonEmpty) {
      suspensionsOld.enqueue(suspensionsYoung.dequeue())
    }

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
   * Attempt to evaluate suspensions on the provie queue. Keep repeating the
   * evaluates as long as some progress is being made. Suspensions that
   * evaluate sucessfully are removed from the queue. Once suspensions make no
   * further progress and are all blocked, we return. Blocked suspensions put
   * back on the same queue.
   *
   * filterToBuildResolvable distinguishes a discard-sink sweep (true,
   * only passed by evalBuildResolvableSuspensions - can never write real
   * bytes) from every other caller (false, the default): when true, a
   * suspension whose canResolveWithoutWriting is false is diverted away
   * from the real doTask/runSuspension attempt - skipped and requeued in
   * place, or parked if isWaitingOnLengthState - since a discard-sink
   * traversal could never satisfy it anyway. When false, every
   * not-yet-done suspension gets a real attempt, since only these
   * callers can actually write the bytes it depends on.
   *
   * A suspension is parked (moved to suspensionsParked instead of back
   * onto this queue) only when isWaitingOnLengthState is true AND this
   * is one of the two throttled callers (filterToBuildResolvable or
   * skipLengthStateWaiters) - a guaranteed external retry already
   * exists for it, so re-examining it on the next periodic tick is
   * pure waste. This is narrower than "anything a throttled caller
   * would otherwise skip": suspensions with no targeted wake-up at all
   * (e.g. padding/target-length SuspendableOperations) stay on the
   * skip-and-requeue path instead, so a later, unfiltered (real) sweep
   * still finds them.
   *
   * Parking removes a suspension from the per-tick rotation entirely;
   * only the targeted wake-up itself or foldParkedIntoOld (the two
   * must-attempt-everything paths) ever revisits it. A parked
   * suspension is dequeued and never re-enqueued, so `queue.length`
   * shrinks correctly for the loop's termination bound.
   *
   * A suspension already `isDone` when dequeued is dropped without
   * calling runSuspension again: a queued suspension can now resolve
   * out-of-band via a targeted wake-up while still sitting in this
   * queue (impossible before wake-ups existed) - without this check,
   * dequeuing straight into another runSuspension call would re-run
   * doTask on an already-done suspension, double-applying its side
   * effect and tripping LengthState's "set once" invariants.
   */
  private def evalSuspensionQueue(
    queue: Queue[Suspension],
    filterToBuildResolvable: Boolean = false,
    skipLengthStateWaiters: Boolean = false
  ): Unit = {
    var countOfNotMakingProgress = 0
    while (!queue.isEmpty && countOfNotMakingProgress < queue.length) {
      val s = queue.dequeue()
      if (s.isDone) {
        // resolved out of band; queue got smaller for free, so this
        // counts as progress the same as a successful run below.
        countOfNotMakingProgress = 0
      } else if (
        (filterToBuildResolvable || skipLengthStateWaiters) && s.isWaitingOnLengthState
      ) {
        // Guaranteed external wake-up exists (see the doc comment above):
        // park it, removing it from the rotation entirely. Same
        // dequeue-without-re-enqueue shrink as the isDone branch, so it
        // resets the counter the same way.
        suspensionsParked.enqueue(s)
        countOfNotMakingProgress = 0
      } else if (filterToBuildResolvable && !s.canResolveWithoutWriting) {
        // Discard-sink-only, not length-state-blocked (e.g. padding/
        // SuspendableOperation, no external wake-up): skip-and-requeue
        // instead of parking, so a later, real sweep can pick it up
        // promptly - parking would strand it until the one-time final
        // fold, since nothing else retries it.
        queue.enqueue(s)
        countOfNotMakingProgress += 1
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
