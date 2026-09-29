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

package org.apache.daffodil.unparsers.runtime1

import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.unparsers.*

/**
 * Write-side dispatch for the build/write-prefetch unparse path, by
 * ordinary recursive calls, using `UnparseSharedContext.awaitChild` to
 * advance build wherever a needed child doesn't yet exist or isn't ready.
 */
trait WriteUnparser {

  // Writes containerNode's content via direct recursive calls - "where we
  // are" is just the JVM call stack, not a return-value state machine. May
  // advance build (via awaitChild) until a needed child exists and is
  // ready, then continues where it left off.
  def writeContent(containerNode: DINode, state: UState): Unit

  // The body dispatch shared by unparse() and writeContent() of a
  // combinator that wraps one body unparser. A Nope containerNode means
  // unparse's event-driven path; otherwise bodyUnparser is dispatched to
  // writeContent if it's a WriteUnparser, else plain unparse1.
  protected final def dispatchBody(
    containerNode: Maybe[DINode],
    bodyUnparser: Unparser,
    state: UState
  ): Unit = {
    if (containerNode.isEmpty) {
      bodyUnparser.unparse1(state)
    } else {
      bodyUnparser match {
        case wu: WriteUnparser => wu.writeContent(containerNode.get, state)
        case _ => bodyUnparser.unparse1(state)
      }
    }
  }
}
