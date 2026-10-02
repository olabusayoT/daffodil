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
 * An unparser that unparses from an already-built infoset tree by recursing
 * into it, instead of consuming infoset events. Where a needed child does
 * not exist yet or is not ready, `UnparseSharedContext.awaitChild` advances
 * build until it is.
 */
trait TreeUnparser { self: Unparser =>

  // Unparses containerNode's content from the built tree. Looking up a child
  // may advance build until that child exists and is ready.
  def unparseTree(containerNode: DINode, state: UState): Unit

  /**
   * unparseTree, preceded and followed by the debugger events that unparse1
   * fires around an unparser. Callers use this, not unparseTree, so a
   * debugger sees the same steps as in a single-pass unparse.
   */
  final def unparseTree1(containerNode: DINode, state: UState): Unit =
    withDebuggerEvents(self, state) {
      unparseTree(containerNode, state)
    }

  /**
   * Runs body between the debugger events unparse1 fires around an
   * unparser. For the unparseTree loops that run a sequence's child unparser
   * inline, where unparse1 is never called on it. Inline, so the normal path
   * pays only a flag test.
   */
  protected final inline def withDebuggerEvents(unparser: Unparser, state: UState)(
    inline body: => Unit
  ): Unit = {
    val debugging = state.areDebugging
    val savedProc = state.maybeProcessor
    if (debugging) {
      state.setProcessor(unparser)
      state.dataProc.get.before(state, unparser)
    }
    body
    if (debugging) {
      state.dataProc.get.after(state, unparser)
      if (savedProc.isDefined) state.setMaybeProcessor(savedProc)
    }
  }

  // The body dispatch shared by unparse and unparseTree of a combinator that
  // wraps one body unparser. Nope means event-driven unparse; otherwise the
  // body runs through its own unparseTree1 if it is a TreeUnparser, else
  // through unparse1.
  protected final def dispatchBody(
    containerNode: Maybe[DINode],
    bodyUnparser: Unparser,
    state: UState
  ): Unit = {
    if (containerNode.isEmpty) {
      bodyUnparser.unparse1(state)
    } else {
      bodyUnparser match {
        case tu: TreeUnparser => tu.unparseTree1(containerNode.get, state)
        case _ => bodyUnparser.unparse1(state)
      }
    }
  }
}
