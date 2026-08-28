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

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope

/**
 * Mixed into any object a Suspension can register a targeted wake-up
 * against: lazily allocates its SuspensionWaiter, since most instances of
 * any such host are never suspended on, and exposes the notify/clear/
 * force-retry operations without forcing that allocation just to find it
 * unneeded. transient: for a host that is itself Serializable (part of
 * the compiled schema's saved state, e.g. VariableInstance), none of that
 * saved state should carry a waiter, which only ever holds transient
 * in-progress suspensions; harmless on a host that isn't Serializable.
 */
trait HasSuspensionWaiter {

  @transient private var _suspensionWaiter: SuspensionWaiter = null

  def suspensionWaiter: SuspensionWaiter = {
    if (_suspensionWaiter eq null) {
      _suspensionWaiter = new SuspensionWaiter
    }
    _suspensionWaiter
  }

  protected def notifySuspensionWaiterIfAllocated(
    changedSubTarget: Maybe[AnyRef] = Nope
  ): Unit = {
    if (_suspensionWaiter ne null) {
      _suspensionWaiter.notifySuspensions(changedSubTarget)
    }
  }

  protected def clearSuspensionWaiterIfAllocated(): Unit = {
    if (_suspensionWaiter ne null) {
      _suspensionWaiter.clear()
    }
  }

  protected def forceRetryAllSuspensionsIfAllocated(): Unit = {
    if (_suspensionWaiter ne null) {
      _suspensionWaiter.forceRetryAll()
    }
  }
}
