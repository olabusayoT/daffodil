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

import org.apache.daffodil.api.DataLocation
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
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.TermRuntimeData

/**
 * The "build" side of the build/write unparse split: it walks the infoset
 * events from an actual `InfosetInputter` and builds the infoset tree ahead
 * of the write pass. It needs only the tree state, so it is not a `UState`:
 * it has no output stream, variables or debugger state, and build never
 * writes content.
 *
 * Used only when the `useBuildWritePrefetch` tunable is enabled; otherwise
 * unparsing constructs `UStateMain` exclusively as before.
 */
final class InfosetBuildState(
  private val inputter: InfosetInputter,
  sharedCtx: UnparseSharedContext,
  areDebugging: Boolean
) extends InfosetTreeState
  with SuspensionResolver
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

  // Build runs ahead of write, so freeing a node here would null out a
  // child reference write hasn't read yet; write still frees as normal.
  override def releaseUnneededInfoset: Boolean = false

  override def sharedContext: Maybe[UnparseSharedContext] = One(sharedCtx)

  override def maybeCurrentLocation: Maybe[DataLocation] = Nope

  // Shared, not owned; one SuspensionTracker queue, both build and write
  // see the same one via sharedCtx.
  def suspensionTracker: SuspensionTracker = sharedCtx.suspensionTracker

  /**
   * Uses evalBuildResolvableSuspensions: a suspension that
   * canResolveWithoutWriting marks false (usually a forward reference) is
   * skipped rather than genuinely retried, and left pending for a later,
   * unfiltered sweep.
   */
  def evalSuspensions(isFinal: Boolean): Unit = {
    // A debugger needs suspensions to resolve at the steps a single-pass
    // unparse would resolve them, which only write's sweeps give it.
    if (!areDebugging) sharedCtx.suspensionTracker.evalBuildResolvableSuspensions()
    if (isFinal) sharedCtx.suspensionTracker.requireFinal()
  }
  def suspensions: Seq[Suspension] = sharedCtx.suspensionTracker.suspensions
}
