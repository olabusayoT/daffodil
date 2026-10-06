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
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * The "build" side of the build/unparseTree split: it walks the infoset
 * events from an actual `InfosetInputter` and builds the infoset tree ahead
 * of unparseTree. It needs only the tree state, so it is not a `UState`:
 * it has no output stream, variables or debugger state, and build never
 * writes content.
 *
 * Used only when the `useBuildPrefetch` tunable is enabled when unparsing;
 * otherwise only `UStateMain` is constructed.
 */
final class InfosetBuildState(
  private val inputter: InfosetInputter,
  sharedCtx: UnparseSharedContext
) extends InfosetTreeState
  with TraversalIndexStacks {

  override def tunable: DaffodilTunables = sharedCtx.tunable

  private val eventState: InfosetEventState = new InputterEventState(inputter, "building")

  override def advance: Boolean = eventState.advance
  override def advanceAccessor: InfosetAccessor = eventState.advanceAccessor
  override def inspect: Boolean = eventState.inspect
  override def inspectAccessor: InfosetAccessor = eventState.inspectAccessor
  override def fini(): Unit = Assert.usageError("Not to be used on InfosetBuildState")
  override def inspectOrError: InfosetAccessor = eventState.inspectOrError
  override def advanceOrError: InfosetAccessor = eventState.advanceOrError
  override def isInspectArrayEnd: Boolean = eventState.isInspectArrayEnd

  override def pushTRD(trd: TermRuntimeData): Unit = eventState.pushTRD(trd)
  override def maybeTopTRD(): Maybe[TermRuntimeData] = eventState.maybeTopTRD()
  override def popTRD(trd: TermRuntimeData): TermRuntimeData = eventState.popTRD(trd)

  override def documentElement: DIDocument = inputter.documentElement

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode]

  override def currentInfosetNode: DINode = {
    if (currentInfosetNodeMaybe.isEmpty) {
      null
    } else {
      currentInfosetNodeMaybe.get
    }
  }

  override def currentInfosetNodeMaybe: Maybe[DINode] = {
    if (currentInfosetNodeStack.isEmpty) {
      Nope
    } else {
      currentInfosetNodeStack.top
    }
  }

  // Build tracks child position in its own frames, never in a stack.
  override def moveOverOneElementChildOnly(): Unit = ()

  private var hiddenDepth = 0
  override def incrementHiddenDef(): Unit = hiddenDepth += 1
  override def decrementHiddenDef(): Unit = hiddenDepth -= 1
  override def withinHiddenNest: Boolean = hiddenDepth > 0

  // Build runs ahead of unparseTree, which frees each node once it is done
  // with it, so build leaves freeing to unparseTree.
  override def freeChildIfNoLongerNeeded(parent: DINode, index: Int): Unit = ()

  override def sharedContext: Maybe[UnparseSharedContext] = One(sharedCtx)
}
