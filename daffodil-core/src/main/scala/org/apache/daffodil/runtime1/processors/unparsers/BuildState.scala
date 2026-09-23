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

import java.io.ByteArrayOutputStream
import scala.collection.mutable

import org.apache.daffodil.api
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.io.StringDataInputStreamForUnparse
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.ThrowsSDE
import org.apache.daffodil.lib.util.LocalStack
import org.apache.daffodil.lib.util.MStackOfMaybe
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.Nope
import org.apache.daffodil.runtime1.dpath.UnparserBlocking
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.DataValue.DataValuePrimitive
import org.apache.daffodil.runtime1.infoset.InfosetAccessor
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.processors.DelimiterStackUnparseNode
import org.apache.daffodil.runtime1.processors.EscapeSchemeUnparserHelper
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.VariableInstance
import org.apache.daffodil.runtime1.processors.VariableRuntimeData
import org.apache.daffodil.runtime1.processors.dfa.DFADelimiter

/**
 * A `UState` subclass that consumes an actual `InfosetInputter` and provides
 * live Cursor/TRD/index-stack behavior; this is the "build" side of the
 * build/write unparse split. The write-only surface (delimiter stack,
 * escape scheme cache, and the scratch buffers used for measuring/escaping
 * text) is stubbed to error, since nothing build does should ever touch it;
 * build never writes content.
 *
 * `getDataOutputStream` is NOT stubbed: the suspend path reads it
 * unconditionally, even for read-only (never-actually-written)
 * suspensions, so `BuildState` constructs an actual DOS wrapping a no-op
 * sink purely to satisfy that.
 *
 * Used only when the `useBuildWritePrefetch` tunable is enabled (default
 * false); otherwise unused, and unparsing constructs `UStateMain`
 * exclusively as before.
 */
final class BuildState(
  private val inputter: InfosetInputter,
  sharedCtx: UnparseSharedContext,
  diagnosticsArg: Seq[api.Diagnostic],
  areDebugging: Boolean
) extends UState(
    sharedCtx.variableBox,
    diagnosticsArg,
    Maybe(sharedCtx.dataProc),
    sharedCtx.tunable,
    areDebugging
  )
  with SuspensionCapableUState
  with LiveIndexStacks {

  dState.setMode(UnparserBlocking)
  setSharedContext(sharedCtx)

  // Purely to satisfy the unconditional getDataOutputStream read on
  // the suspend path (see class doc above). Never actually written
  // to: build's suspensions are always read-only
  // (maybeKnownLengthInBits == 0), so splitDOS never runs for them.
  setDataOutputStream(
    DirectOrBufferedDataOutputStream(
      new java.io.OutputStream { override def write(b: Int): Unit = () },
      null,
      false,
      sharedCtx.tunable.outputStreamChunkSizeInBytes,
      sharedCtx.tunable.maxByteArrayOutputStreamBufferSizeInBytes,
      sharedCtx.tunable.tempFilePath
    )
  )
  // Sets up bit order the same way single-pass unparse does, for
  // parity with write/single-pass, even though build's own isBuildOnly
  // gates mean it never actually needs to check bit order during build.
  getDataOutputStream.setPriorBitOrder(
    sharedCtx.dataProc.ssrd.elementRuntimeData.defaultBitOrder
  )

  override def isBuildOnly: Boolean = true

  private def writeOnly =
    Assert.usageError("BuildState never writes content, so this write-only state doesn't exist")

  override def escapeSchemeEVCache: MStackOfMaybe[EscapeSchemeUnparserHelper] = writeOnly
  override def withUnparserDataInputStream: LocalStack[StringDataInputStreamForUnparse] =
    writeOnly
  override def withByteArrayOutputStream
    : LocalStack[(ByteArrayOutputStream, DirectOrBufferedDataOutputStream)] = writeOnly
  override def allTerminatingMarkup: List[DFADelimiter] = writeOnly
  override def localDelimiters: DelimiterStackUnparseNode = writeOnly
  override def pushDelimiters(node: DelimiterStackUnparseNode): Unit = writeOnly
  override def popDelimiters(): Unit = writeOnly

  override def advance: Boolean = inputter.advance
  override def advanceAccessor: InfosetAccessor = inputter.advanceAccessor
  override def inspect: Boolean = inputter.inspect
  override def inspectAccessor: InfosetAccessor = inputter.inspectAccessor
  override def fini(): Unit = Assert.usageError("Not to be used on UState")

  override def inspectOrError: InfosetAccessor = {
    if (inspect) {
      inspectAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for building, but no InfosetEvent was available."
      )
    }
  }

  override def advanceOrError: InfosetAccessor = {
    if (advance) {
      advanceAccessor
    } else {
      Assert.invariantFailed(
        "An InfosetEvent was required for building, but no InfosetEvent was available."
      )
    }
  }

  override def isInspectArrayEnd: Boolean = {
    if (!inspect) {
      false
    } else {
      inspectAccessor match {
        case e if e.isEnd && e.isArray => true
        case _ => false
      }
    }
  }

  def currentInfosetNode: DINode = {
    if (currentInfosetNodeMaybe.isEmpty) {
      null
    } else {
      currentInfosetNodeMaybe.get
    }
  }

  def currentInfosetNodeMaybe: Maybe[DINode] = {
    if (currentInfosetNodeStack.isEmpty) {
      Nope
    } else {
      currentInfosetNodeStack.top
    }
  }

  override val currentInfosetNodeStack = new MStackOfMaybe[DINode]

  /**
   * Build-local NVI (`dfdl:newVariableInstance`) scope tracking, keyed by
   * `vmapIndex`, closing each scope the moment build's own traversal exits
   * it. Kept separate from the shared `VariableMap`/`vTable`, which only
   * write pops, much later, so the shared array's head can lag behind.
   */
  private val nviLocalStacks: mutable.Map[Int, List[VariableInstance]] = mutable.Map.empty

  /**
   * Pushes onto the shared vTable exactly as the base implementation
   * does, then additionally records the same instance on build's own
   * local scope stack.
   */
  override def newVariableInstance(vrd: VariableRuntimeData): VariableInstance = {
    val nvi = super.newVariableInstance(vrd)
    val idx = vrd.vmapIndex
    nviLocalStacks(idx) = nvi :: nviLocalStacks.getOrElse(idx, Nil)
    nvi
  }

  /**
   * Pops build's own local scope stack only; the shared vTable is left
   * untouched here, since write's own end-of-scope handling pops that,
   * later, via the unmodified base `removeVariableInstance`.
   */
  override def removeVariableInstance(vrd: VariableRuntimeData): Unit = {
    val idx = vrd.vmapIndex
    val stack = nviLocalStacks.getOrElse(idx, Nil)
    Assert.invariant(stack.nonEmpty)
    nviLocalStacks(idx) = stack.tail
  }

  /**
   * Resolves against whichever NVI instance build currently has open,
   * rather than the shared vTable's head (which can lag behind). Falls
   * back to the original (pre-NVI) instance once build has locally
   * exited every NVI scope for this vmapIndex.
   */
  override def getVariable(
    vrd: VariableRuntimeData,
    referringContext: ThrowsSDE
  ): DataValuePrimitive = {
    variableMap.checkDirectionForRead(vrd, this)
    nviLocalStacks.getOrElse(vrd.vmapIndex, Nil) match {
      case head :: _ => variableMap.readVariable(head, vrd, referringContext, this)
      case Nil =>
        variableMap.readVariable(
          variableMap.originalInstanceAt(vrd.vmapIndex),
          vrd,
          referringContext,
          this
        )
    }
  }

  /**
   * Mirrors getVariable above: must land on whichever NVI instance build
   * currently has open, not the shared vTable's head; write's deferred
   * End pop can race ahead of it, tripping a spurious "cannot set
   * variable twice" SDE on the wrong scope.
   */
  override def setVariable(
    vrd: VariableRuntimeData,
    newValue: DataValuePrimitive,
    referringContext: ThrowsSDE
  ): Unit = {
    nviLocalStacks.getOrElse(vrd.vmapIndex, Nil) match {
      case head :: _ =>
        variableMap.setVariable(head, vrd, newValue, referringContext, this)
      case Nil =>
        variableMap.setVariable(
          variableMap.originalInstanceAt(vrd.vmapIndex),
          vrd,
          newValue,
          referringContext,
          this
        )
    }
  }

  // Shared, not owned; one SuspensionTracker queue, both build and write
  // see the same one via sharedCtx.
  def suspensionTracker: SuspensionTracker = sharedCtx.suspensionTracker
  def addSuspension(se: Suspension): Unit = sharedCtx.suspensionTracker.trackSuspension(se)

  /**
   * Uses evalBuildResolvableSuspensions: canResolveWithoutWriting is a
   * static, direction-blind heuristic, so a suspension it marks false
   * (usually a forward reference, occasionally an already-resolved
   * backward one) is skipped here rather than genuinely retried, and
   * left pending for a later, unfiltered sweep instead.
   */
  def evalSuspensions(isFinal: Boolean): Unit = {
    sharedCtx.suspensionTracker.evalBuildResolvableSuspensions()
    if (isFinal) sharedCtx.suspensionTracker.requireFinal()
  }
  def suspensions = sharedCtx.suspensionTracker.suspensions

  /**
   * Simpler than the base implementation's suspension clone: no
   * escape-scheme/delimiter state to clone. Patches the clone's vTable,
   * per open NVI scope, to the SAME instance build's own read resolved
   * against, not the shared array's raw head, or every retry would
   * target the wrong, stale scope.
   */
  override def cloneForSuspension(suspendedDOS: DirectOrBufferedDataOutputStream): UState = {
    val clonedBox = sharedCtx.variableBox.cloneForSuspension()
    nviLocalStacks.foreach {
      case (idx, head :: _) => clonedBox.vmap.overrideHeadForSuspensionClone(idx, head)
      case (idx, Nil) =>
        // Build locally exited every NVI scope for this index; target the
        // original instance the read resolved against, not whatever the
        // shared array's raw head happens to be.
        clonedBox.vmap.overrideHeadForSuspensionClone(idx, variableMap.originalInstanceAt(idx))
    }
    val clone = new UStateForSuspension(
      this,
      suspendedDOS,
      clonedBox,
      currentInfosetNodeStack.top.get,
      arrayIterationIndexStack.top,
      occursIndexStack.top,
      Nope,
      Nope,
      sharedCtx.tunable,
      areDebugging
    )
    clone.setProcessor(processor)
    clone
  }

  final override def pushTRD(trd: TermRuntimeData): Unit = inputter.pushTRD(trd)
  final override def maybeTopTRD(): Maybe[TermRuntimeData] = inputter.maybeTopTRD()
  final override def popTRD(trd: TermRuntimeData): TermRuntimeData = {
    val poppedTRD = inputter.popTRD()
    if (poppedTRD ne trd)
      Assert.invariantFailed("TRDs do not match. Expected: " + trd + " got " + poppedTRD)
    poppedTRD
  }

  final override def documentElement: DIDocument = inputter.documentElement
}
