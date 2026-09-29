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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.iapi.DaffodilTunables
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker

/**
 * What build and write genuinely need to share by reference: the
 * `SuspensionTracker` (one queue; build opportunistically resolves
 * write-created suspensions early against the tree it has already
 * built, and write drains whatever remains at the end).
 *
 * Also owns the lead counter: how far build is ahead of write,
 * incremented once per node build constructs and decremented once per
 * node write finishes. Write pulls build forward through `buildCursor`
 * whenever it needs tree that does not exist yet; build stops advancing
 * once `leadExceedsPrefetchLimit` or the pending-suspension backlog
 * exceeds `pendingSuspensionTripLimit` (below), bounding how far ahead
 * it may run. Both sides run on the same thread.
 */
final class UnparseSharedContext(
  val suspensionTracker: SuspensionTracker,
  val dataProc: DataProcessor,
  val tunable: DaffodilTunables,
  val prefetchLimit: Long
) {
  private var buildLead: Long = 0

  def incrementLead(): Unit = buildLead += 1

  def decrementLead(): Unit = {
    buildLead -= 1
    Assert.invariant(buildLead >= 0)
  }

  def currentLead: Long = buildLead

  def leadExceedsPrefetchLimit: Boolean = buildLead > prefetchLimit

  /**
   * A second, independent limit on how far build may run ahead of write,
   * alongside prefetchLimit: a suspension can be created without moving
   * the lead counter, so the lead alone doesn't bound how many pile up
   * pending.
   */
  def pendingSuspensionTripLimit: Long = tunable.unparsePendingSuspensionTripLimit

  private var buildCursor_ : BuildCursor = null

  def setBuildCursor(bc: BuildCursor): Unit = buildCursor_ = bc

  /**
   * Called from write when suspensions pile up faster than write's own
   * progress resolves them: builds a little further ahead, which gives the
   * build-side sweeps more tree to resolve them against. Does nothing once
   * the lead window is already full or build has finished.
   */
  def relieveSuspensionBacklog(): Unit = {
    if (
      buildCursor_ != null && !buildCursor_.isFinished && !leadExceedsPrefetchLimit &&
      suspensionTracker.pendingCount > pendingSuspensionTripLimit
    ) {
      buildCursor_.advance()
    }
  }

  /**
   * True if `child` may safely be written now: complex/array existing is
   * enough; simple needs a value, except hidden/nilled elements (never
   * given one) and OVC (deferred via its own Suspension; must not block,
   * or an OVC depending on a later sibling's write-time property would deadlock).
   */
  private def isChildReady(child: DINode): Boolean = {
    if (child.isSimple) {
      val s = child.asSimple
      child.isHidden || s.isNilled || s.hasValue || s.erd.dpathElementCompileInfo.isOutputValueCalc
    } else {
      // complex/array, hidden or not; existing is enough
      true
    }
  }

  /**
   * One attempt at unblocking write: advances build if it is unfinished,
   * else retries suspensions. False means no progress was made, so callers
   * must throw AwaitChildStalledException; the final suspension sweep
   * gives the real diagnosis.
   */
  private def tryUnblockWrite(): Boolean = {
    if (buildCursor_ != null && !buildCursor_.isFinished) {
      buildCursor_.advance()
      true
    } else {
      val before = suspensionTracker.suspensions.length
      suspensionTracker.evalSuspensionsUnthrottled()
      suspensionTracker.suspensions.length < before
    }
  }

  /**
   * Advances build until `parent.child(index)` both exists and is ready,
   * throwing AwaitChildStalledException once no further progress is
   * possible.
   */
  def awaitChild(parent: DINode, index: Int): DINode = {
    while (index >= parent.numChildren || !isChildReady(parent.child(index))) {
      if (!tryUnblockWrite()) throw new AwaitChildStalledException
    }
    parent.child(index)
  }

  /**
   * Advances build until `parent.child(index)` exists, or `parent.isFinal`
   * with no child there; for callers asking "is there more data at all"
   * rather than awaiting a child known to be coming. A true result may
   * still need awaitChild afterward for value readiness.
   */
  def childExistsOrFinal(parent: DINode, index: Int): Boolean = {
    while (index >= parent.numChildren) {
      if (parent.isFinal) return false
      if (!tryUnblockWrite()) throw new AwaitChildStalledException
    }
    true
  }
}

/**
 * Thrown when write can make no further progress after build has
 * finished and an unthrottled suspension retry made no headway. Caught
 * only by the top-level driver, which still runs its normal
 * finalization (the genuine, diagnostic-producing final suspension
 * drain) rather than treating this as the final outcome itself.
 */
final class AwaitChildStalledException extends Exception

/**
 * Thrown from inside write's recursion when advancing build failed; the
 * original build failure is the cause. Caught only by the top-level
 * driver, which skips its own normal finalization entirely (those
 * invariants and the final suspension drain assume a consistently,
 * fully-built tree that a failed build may not have produced) and
 * reports the cause rather than this exception.
 */
final class BuildAbortedException(cause: Throwable) extends Exception(cause)
