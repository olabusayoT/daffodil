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
 * now succeed, never a guarantee - the state that changed might not be
 * the thing this particular waiter's suspensions actually need. Override
 * notifySuspensions() to re-verify before calling super.notifySuspensions()
 * wherever that distinction matters; the default just unparks
 * unconditionally, which is correct when the caller already knows the
 * state is fully resolved (e.g. a variable that was just set).
 */
class SuspensionWaiter {

  private val suspensions: mutable.HashSet[Suspension] = mutable.HashSet.empty

  private[runtime1] def registerSuspension(s: Suspension): Unit = {
    suspensions.add(s)
  }

  private[runtime1] def removeSuspension(s: Suspension): Unit = {
    suspensions.remove(s)
  }

  private[runtime1] def isRegisteredSuspension(s: Suspension): Boolean =
    suspensions.contains(s)

  def isEmpty: Boolean = suspensions.isEmpty

  def clear(): Unit = suspensions.clear()

  // Hands every waiter to its tracker (Suspension.moveFromParkedToYoung)
  // for a real attempt on its next sweep; SuspensionTracker is the only
  // caller of runSuspension.
  def notifySuspensions(): Unit = {
    if (suspensions.nonEmpty) {
      val toMove = suspensions.toArray
      suspensions.clear()
      toMove.foreach { s => if (!s.isDone) s.moveFromParkedToYoung() }
    }
  }
}
