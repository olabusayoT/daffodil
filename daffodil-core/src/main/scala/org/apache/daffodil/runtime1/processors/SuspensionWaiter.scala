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

/**
 * Tracks suspensions parked waiting on some state change that might make
 * them resolvable (a LengthState's length becoming known, a variable
 * being set, etc.), and moves them back to young once notified.
 *
 * A call to notifySuspensions() is only ever a hint that a retry might
 * now succeed, never a guarantee; the state that changed might not be
 * the thing this particular registrant actually needs, nor even the same
 * fact a different registrant on this same waiter needs. Each
 * registration carries its own condition, re-verified here before that
 * one registrant is woken; the default (registerSuspension's cond
 * defaults to always-true) wakes unconditionally, correct when every
 * registrant on a waiter wants the same already-resolved fact.
 */
class SuspensionWaiter {

  // A given waiter (one per LengthState/variable instance) overwhelmingly
  // has at most one registrant across its whole lifetime, held directly
  // here with no Map at all. overflow is allocated only for a second,
  // distinct registrant, and stays allocated even if it later shrinks
  // back to one, since this waiter's own lifetime is already bounded.
  private var soleSuspension: Suspension = null
  private var soleCond: () => Boolean = null
  private var overflow: mutable.LinkedHashMap[Suspension, () => Boolean] = null

  private[processors] def registerSuspension(
    s: Suspension,
    cond: () => Boolean = () => true
  ): Unit = {
    if (overflow ne null) {
      overflow.put(s, cond)
    } else if (soleSuspension eq null) {
      soleSuspension = s
      soleCond = cond
    } else if (soleSuspension eq s) {
      // Re-registering the suspension already holding the sole slot is a
      // plain field update, no allocation; this is what makes a caller
      // that calls registerWaiter unconditionally on every retry (e.g.
      // SuspendableOperation's maybeRegisterWaiterOnBlock) cheap here.
      soleCond = cond
    } else {
      // A second, distinct registrant: promote to the map, preserving
      // insertion order (the sole registrant was registered first).
      overflow = mutable.LinkedHashMap.empty
      overflow.put(soleSuspension, soleCond)
      overflow.put(s, cond)
      soleSuspension = null
      soleCond = null
    }
  }

  private[processors] def removeSuspension(s: Suspension): Unit = {
    if (overflow ne null) {
      overflow.remove(s)
    } else if (soleSuspension eq s) {
      soleSuspension = null
      soleCond = null
    }
  }

  def isRegisteredSuspension(s: Suspension): Boolean =
    if (overflow ne null) overflow.contains(s)
    else soleSuspension eq s

  def isEmpty: Boolean =
    if (overflow ne null) overflow.isEmpty
    else soleSuspension eq null

  // Drops every registered suspension without notifying them. Only safe
  // because clearRegisteredWaiter() resets their own back-reference too,
  // so none is left believing it still has a wake-up armed here.
  def clear(): Unit = {
    if (overflow ne null) {
      overflow.keysIterator.foreach(_.clearRegisteredWaiter())
      overflow.clear()
    } else if (soleSuspension ne null) {
      soleSuspension.clearRegisteredWaiter()
      soleSuspension = null
      soleCond = null
    }
  }

  // Hands every registrant whose own condition now holds to its tracker
  // (Suspension.moveFromParkedToYoung) for a real attempt; one whose
  // condition doesn't hold yet stays registered for a later notify. A
  // done suspension is dropped either way: it's already resolved.
  def notifySuspensions(): Unit = {
    // Once allocated, overflow stays allocated even after every
    // registrant resolves, so the nonEmpty check guards every later
    // notify on a waiter that's done suspending for good from paying
    // for nothing.
    if ((overflow ne null) && overflow.nonEmpty) {
      // Materialized fully before any removal below: moveFromParkedToYoung
      // can reach back into this same map's register/removeSuspension,
      // so mutating overflow while still iterating a live view over it
      // isn't safe.
      val handled = overflow.iterator.collect {
        case (s, cond) if s.isDone || cond() => s
      }.toList
      handled.foreach(overflow.remove)
      handled.foreach { s =>
        if (!s.isDone) {
          s.moveFromParkedToYoung()
        }
      }
    } else if (soleSuspension ne null) {
      val s = soleSuspension
      val cond = soleCond
      // s.isDone || cond() can reenter this same notifySuspensions() call
      // before this line returns (e.g. cond() migrates a position that
      // notifies this same waiter); moveFromParkedToYoung's own
      // !isParked guard makes the resulting duplicate call a no-op.
      if (s.isDone || cond()) {
        soleSuspension = null
        soleCond = null
        if (!s.isDone) {
          s.moveFromParkedToYoung()
        }
      }
    }
  }

  // Hands every registrant a real retry regardless of its own condition,
  // for when the tracked fact has become permanently stale rather than
  // merely still-pending: retrying naturally re-resolves against
  // whatever's current instead of waiting forever on a stale one.
  def forceRetryAll(): Unit = {
    if ((overflow ne null) && overflow.nonEmpty) {
      val toRetry = overflow.keysIterator.toList
      overflow.clear()
      toRetry.foreach { s =>
        if (!s.isDone) {
          s.moveFromParkedToYoung()
        }
      }
    } else if (soleSuspension ne null) {
      val s = soleSuspension
      soleSuspension = null
      soleCond = null
      if (!s.isDone) {
        s.moveFromParkedToYoung()
      }
    }
  }
}
