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

import org.apache.daffodil.io.BitOrderChangeException
import org.apache.daffodil.io.DataOutputStream
import org.apache.daffodil.io.DataOutputStreamEventListener
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.*
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.infoset.InfosetArrayIndexOutOfBoundsException
import org.apache.daffodil.runtime1.infoset.InfosetLengthUnknownException
import org.apache.daffodil.runtime1.infoset.InfosetNoNextSiblingException
import org.apache.daffodil.runtime1.infoset.InfosetNoSuchChildElementException
import org.apache.daffodil.runtime1.infoset.InfosetNodeNotFinalException
import org.apache.daffodil.runtime1.infoset.OutputValueCalcEvaluationException
import org.apache.daffodil.runtime1.infoset.RetryableException
import org.apache.daffodil.runtime1.processors.unparsers.UState
import org.apache.daffodil.runtime1.processors.unparsers.UStateMain
import org.apache.daffodil.runtime1.processors.unparsers.UnparseError

/**
 * The suspension object keeps track of the state of the task, i.e., whether it
 * is done, whether it is making forward progress when run or not.
 *
 * A suspension" may block, by which we mean it may set isDone to false, and return.
 *
 * Running the suspension again tries again and will either block or complete.
 *
 */
trait Suspension extends Serializable with DataOutputStreamEventListener {

  /**
   * Specifies that this suspension does not write to the data output stream.
   *
   * Override in TargetLengthOperation,and in SuspendableExpression as they
   * don't write to the DOS hence, if a DOS is created it can be setFinished
   * immediately.
   *
   * TODO: Redundant with implementing maybeKnownLengthInBits as MaybeULong(0L)
   */
  val isReadOnly = false

  def UE(ustate: UState, s: String, args: Any*) = {
    UnparseError(One(rd.schemaFileLocation), One(ustate.currentLocation), s, args*)
  }

  private var savedUstate_ : UState = null

  final def savedUstate = {
    Assert.invariant(savedUstate_ ne null)
    // Assert above fails if no suspension state created yet. Can't ask for ustate.
    // means we are pre-evaluating to decide if we need to suspend.
    savedUstate_
  }

  def rd: RuntimeData

  protected def maybeKnownLengthInBits(ustate: UState): MaybeULong = MaybeULong.Nope

  protected def doTask(ustate: UState): Unit

  /**
   * After calling this, call isDone and if that's false call isMakingProgress
   * to understand whether it is done, blocked on the exactly same situation,
   * or blocked elsewhere. Needed for circular deadlock detection.
   */
  final def runSuspension(): Unit = {
    doTask(savedUstate)
    if (isDone && !isReadOnly) {
      try {
        //
        // We are done, and we're not readOnly, so the
        // DOS needs to be set finished now.
        //
        savedUstate.getDataOutputStream.setFinished(savedUstate)
      } catch {
        case boc: BitOrderChangeException =>
          savedUstate.SDE(boc)
      }
      Logger.log.debug(s"${this} finished ${savedUstate}.")
    }
  }

  /**
   * Run the first time.
   *
   */
  final def run(ustate: UState): Unit = {
    doTask(ustate)
    if (!isDone) {
      prepareToSuspend(ustate)
    }
  }

  // True only during prepareToSuspend below, before suspend() has assigned
  // savedUstate_: a notify that reenters in that window (e.g. a side
  // effect of evaluating maybeKnownLengthInBits) can't call
  // moveFromParkedToYoung yet, so it's deferred here instead of crashing
  // on savedUstate's own null check.
  private var preparingToSuspend_ : Boolean = false
  private var deferredWakeupWhilePreparing_ : Boolean = false

  private def prepareToSuspend(ustate: UState): Unit = {
    preparingToSuspend_ = true
    try {
      val mkl = maybeKnownLengthInBits(ustate)
      //
      // It seems like we have too many splits going on.
      //
      // As written, we have a bunch of suspensions that occur, but have
      // specifically known length of zero bits. So nothing being written out.
      // In that case, why do we need to split at all?
      //
      val original = ustate.getDataOutputStream
      if (mkl.isEmpty || (mkl.isDefined && mkl.get > 0)) {
        //
        // only split if the length is either unknown
        // or known and greater than 0.
        //
        // If length known 0, then no need for another DOS
        //
        splitDOS(ustate, mkl, original)
      }
      suspend(ustate, original)
    } finally {
      preparingToSuspend_ = false
    }
    if (deferredWakeupWhilePreparing_) {
      deferredWakeupWhilePreparing_ = false
      moveFromParkedToYoung()
    }
  }

  private def splitDOS(
    ustate: UState,
    maybeKnownLengthInBits: MaybeULong,
    original: DirectOrBufferedDataOutputStream
  ): Unit = {
    Assert.usage(ustate.currentInfosetNodeMaybe.isDefined)

    val buffered = original.addBuffered()

    if (maybeKnownLengthInBits.isDefined) {
      // We know the length of the unparsed representation of this suspension that we
      // are currently skipping. Use that length to give hints to this DOS or the new
      // split DOS that can used to set absolute bit positions and avoid deadlocks
      // since absolute bit positions are usually needed to evaluate suspensions
      // (e.g. alignment, length calculations).
      val suspensionLength = maybeKnownLengthInBits.getULong

      if (original.maybeAbsBitPos0b.isDefined) {
        // We know the absolute bitPosition of the original dataOutputStream. That
        // means we can just add the known length of this suspension to that and set
        // it as the starting absolute bit position of the new split buffer.
        val originalAbsBitPos0b = original.maybeAbsBitPos0b.getULong
        buffered.setAbsStartingBitPos0b(originalAbsBitPos0b + suspensionLength)
        buffered.setPriorBitOrder(ustate.bitOrder)
      } else {
        // We do not know the absolute position of the original buffer. This means we
        // don't yet know where the new buffer starts. However, we can calculate the
        // final length of the original DOS (relative position + suspension length),
        // and set that as length of the original DOS. Once that DOS learns its
        // absolute starting position, the DOS length can be used to set the absolute
        // starting position of the split DOS.
        val originalRelBitPos0b = original.relBitPos0b
        original.setLengthInBits(originalRelBitPos0b + suspensionLength)
      }
    } else {
      Logger.log.debug(
        s"Buffered DOS created for ${ustate.currentInfosetNode.erd.diagnosticDebugName} without knowning absolute start bit pos: ${buffered}"
      )
    }

    // the main-thread will carry on using the original ustate but unparsing
    // into this buffered stream.
    ustate.setDataOutputStream(buffered)
  }

  private def suspend(ustate: UState, original: DirectOrBufferedDataOutputStream): Unit = {
    //
    // clone the ustate for use when evaluating the expression
    //
    // This is a targeted partial clone (shallow VariableMap copy, stack
    // tops only), not a full deep copy, but still copies the full
    // escapeSchemeEVCache/delimiterStack contents unconditionally.
    // TODO: Performance: a copy-on-write scheme could avoid that copy.
    //
    val didSplit = (ustate.getDataOutputStream ne original)
    val cloneUState = ustate.asInstanceOf[UStateMain].cloneForSuspension(original)
    if (isReadOnly && didSplit) {
      Assert.invariantFailed("Shouldn't have split. read-only case")
    }

    savedUstate_ = cloneUState

    ustate.asInstanceOf[UStateMain].addSuspension(this)
  }

  /**
   * Called once a registered wake-up (a SuspensionWaiter's
   * notifySuspensions, or this suspension's own notifyKnown) confirms a
   * real attempt is worth trying again: hands it to its tracker, clearing
   * isParked first so it isn't immediately re-parked.
   */
  final def moveFromParkedToYoung(): Unit = {
    if (!isParked) {
      // A condition's own re-verification can cascade into resolving
      // another registrant on the same waiter before this call's own
      // classification was made; that reentrant notify already moved
      // this suspension once, so this stale, now-redundant call is a
      // no-op rather than an error.
    } else if (preparingToSuspend_) {
      deferredWakeupWhilePreparing_ = true
    } else {
      // A suspension can be registered against both a SuspensionWaiter and
      // a DataOutputStream at once. Whichever fired to trigger this call
      // already cleared itself, so this is redundant (and harmless) for
      // that one, but still needed for any other still-registered kind.
      clearAllRegistrations()
      savedUstate.suspensionTracker.moveParkedToYoung(this)
    }
  }

  final def explain(): Unit = {
    val t = this
    Assert.invariant(t.isBlocked)
    Logger.log.warn(s"${t.blockedLocation}")
  }

  private var priorNodeOrVar: Maybe[AnyRef] = Nope
  private var priorInfo: Maybe[AnyRef] = Nope
  private var priorIndex: MaybeInt = MaybeInt.Nope
  private var priorExc: Maybe[AnyRef] = Nope

  private var maybeNodeOrVar: Maybe[AnyRef] = Nope
  private var maybeInfo: Maybe[AnyRef] = Nope
  private var maybeIndex: MaybeInt = MaybeInt.Nope
  private var maybeExc: Maybe[AnyRef] = Nope

  private var done_ : Boolean = false
  private var isBlocked_ = false

  final def setDone(): Unit = {
    done_ = true
    // A suspension can resolve via the periodic fallback retry rather than
    // via its registered wake-up firing (moveFromParkedToYoung, which
    // already clears these). Deregister here too so it doesn't stay
    // reachable from something that will never notify it again.
    clearAllRegistrations()
  }

  final def isDone = done_

  final def isBlocked = isBlocked_

  // Set only for the duration of evalParkedSuspensions' own force-retry
  // pass over this suspension, so moveParkedToYoung can distinguish a
  // reentrant notify mid-pass from an ordinary wake-up: suspensionsParked
  // membership alone doesn't signal which retry pass, if any, is live.
  private var isMidForcedRetry_ : Boolean = false

  private[processors] def markMidForcedRetry(): Unit = { isMidForcedRetry_ = true }

  private[processors] def clearMidForcedRetry(): Unit = { isMidForcedRetry_ = false }

  private[processors] def isMidForcedRetry: Boolean = isMidForcedRetry_

  final def setUnblocked(): Unit = {
    isBlocked_ = false
  }

  /**
   * False if the expression blocked at the same spot, i.e.,
   * didn't make any forward progress.
   */
  private var isMakingProgress_ : Boolean = true

  final def isMakingProgress = isMakingProgress_

  // Which SuspensionWaiter (if any) this suspension is registered with.
  // maybeRegisterWaiterFor reconciles this against what a new block()
  // call needs, so a re-block on the same reason finds it already
  // correctly set.
  private var maybeRegisteredWaiter: Maybe[SuspensionWaiter] = Nope

  // For a maybeRegisterWaiterFor case whose condition depends on more
  // than just the waiter's identity (a specific child name or array
  // index sharing one DIComplex/DIArray's waiter with other possible
  // targets): the specific sub-target that must also match for a
  // re-block to count as already correctly registered, Nope when the
  // waiter identity alone is enough.
  private var maybeRegisteredSubTarget: Maybe[AnyRef] = Nope

  // Tracks this suspension's direct registrations against DataOutputStreams
  // for one of their facts settling into its final value; not a shared
  // per-element waiter like maybeRegisteredWaiter. Each suspension may be
  // watching different DOSs from its own current writing context, and may
  // need more than one at once (e.g. a length calculation registering on
  // both its start and end DOS), which is why this is a Set rather than a
  // single Maybe. One registry covers every such fact: a suspension that
  // only cares about one of them just re-checks that specific fact when
  // notified, rather than needing a separate registry per fact.
  final class DosRegistrations(
    register: DataOutputStream => Unit,
    deregister: DataOutputStream => Unit
  ) {
    private var doses: Set[DataOutputStream] = Set.empty

    def nonEmpty: Boolean = doses.nonEmpty

    def registerFor(dos: DataOutputStream): Unit = {
      if (!doses.contains(dos)) {
        doses = doses + dos
        register(dos)
      }
    }

    def clear(): Unit = {
      doses.foreach(deregister)
      doses = Set.empty
    }
  }

  // Lazily allocated: many suspensions (e.g. anything resolved via
  // registerWaiter alone, or before ever blocking on a DOS fact) never
  // register a DOS listener, so building this and its two closures for
  // every suspension would be wasted work.
  private var _dosListeners: DosRegistrations = null

  final def dosListeners: DosRegistrations = {
    if (_dosListeners eq null) {
      _dosListeners = new DosRegistrations(_.registerListener(this), _.removeListener(this))
    }
    _dosListeners
  }

  // Deregisters from the SuspensionWaiter (if any) and every DOS
  // registration, without resolving or moving this suspension. Checks
  // the backing field directly, not the dosListeners accessor, so a
  // suspension that's never needed one doesn't force the allocation.
  private[processors] def clearAllRegistrations(): Unit = {
    if (maybeRegisteredWaiter.isDefined) {
      maybeRegisteredWaiter.get.removeSuspension(this)
      maybeRegisteredWaiter = Nope
    }
    if (_dosListeners ne null) {
      _dosListeners.clear()
    }
  }

  /**
   * True exactly when a targeted wake-up is registered against some
   * SuspensionWaiter (registerWaiter) or DataOutputStream (dosListeners).
   * SuspensionTracker parks a suspension with this true instead of
   * attempting it.
   */
  final def isParked: Boolean =
    maybeRegisteredWaiter.isDefined || ((_dosListeners ne null) && _dosListeners.nonEmpty)

  final def registerWaiter(
    w: SuspensionWaiter,
    cond: () => Boolean = () => true,
    subTarget: Maybe[AnyRef] = Nope
  ): Unit = {
    Assert.invariant(maybeRegisteredWaiter.isEmpty)
    maybeRegisteredWaiter = One(w)
    w.registerSuspension(this, cond, subTarget)
  }

  // DataOutputStreamEventListener's callback: some registered fact about
  // dos (via dosListeners) is now known, so this suspension is worth a
  // real attempt again.
  final def notifyKnown(dos: DataOutputStream): Unit = {
    if (!isDone) {
      moveFromParkedToYoung()
    }
  }

  // Called only when a waiter is reset (e.g. a LengthState being reused)
  // and drops every suspension it holds without notifying them. Without
  // this, a dropped suspension would keep isParked true forever for a
  // waiter that no longer tracks it.
  private[processors] def clearRegisteredWaiter(): Unit = {
    maybeRegisteredWaiter = Nope
  }

  private def isAlreadyRegisteredOn(
    waiter: SuspensionWaiter,
    subTarget: Maybe[AnyRef]
  ): Boolean =
    maybeRegisteredWaiter.isDefined &&
      (maybeRegisteredWaiter.get eq waiter) &&
      (maybeRegisteredSubTarget == subTarget)

  private def reregister(
    waiter: SuspensionWaiter,
    subTarget: Maybe[AnyRef],
    cond: () => Boolean
  ): Unit = {
    clearAllRegistrations()
    registerWaiter(waiter, cond, subTarget)
    maybeRegisteredSubTarget = subTarget
  }

  /**
   * Registers a targeted wake-up for exc: a re-block on the same target
   * as the current registration is left untouched, for every exception
   * type below, since each one's condition closes over a stable
   * underlying object (the LengthState/VariableInstance/element/array),
   * not the exception instance itself, so an unchanged target implies an
   * unchanged condition too. Each case checks that cheaply before
   * building its condition closure, since a re-block on the same target
   * is the common case.
   */
  private def maybeRegisterWaiterFor(exc: AnyRef): Unit = {
    exc match {
      case noLength: InfosetLengthUnknownException =>
        val waiter = noLength.lengthState.suspensionWaiter
        if (!isAlreadyRegisteredOn(waiter, Nope))
          reregister(waiter, Nope, () => noLength.lengthState.maybeLengthInBits().isDefined)
      // VariableCircularDefinition deliberately excluded: its own
      // evaluation never completes, so nothing will ever notify its
      // suspensionWaiter; registering here would permanently park this
      // suspension instead of letting the periodic sweep keep retrying it.
      case noVar: HasVariableInstance with RetryableException =>
        val variableInstance = noVar.variableInstance
        val waiter = variableInstance.suspensionWaiter
        if (!isAlreadyRegisteredOn(waiter, Nope))
          reregister(waiter, Nope, () => variableInstance.value.isDefined)
      case ovc: OutputValueCalcEvaluationException =>
        // dfdl:outputValueCalc is only ever valid on a simple-type element,
        // so this cast is safe.
        val e = ovc.diElement.asInstanceOf[DISimple]
        val waiter = e.suspensionWaiter
        if (!isAlreadyRegisteredOn(waiter, Nope))
          reregister(waiter, Nope, () => e.hasValue || e.isNilled)
      case unfin: InfosetNodeNotFinalException =>
        val node = unfin.node
        val waiter = node.suspensionWaiter
        if (!isAlreadyRegisteredOn(waiter, Nope))
          reregister(waiter, Nope, () => node.isFinal)
      case noChild: InfosetNoSuchChildElementException =>
        // diComplex's waiter is shared by whatever other child name a
        // different retry of this same suspension might have blocked on
        // before, so nqn must also match for the old registration to
        // still be the right one.
        val diComplex = noChild.diComplex
        val nqn = noChild.nqn
        val waiter = diComplex.suspensionWaiter
        val subTarget: Maybe[AnyRef] = One(nqn)
        if (!isAlreadyRegisteredOn(waiter, subTarget))
          reregister(waiter, subTarget, () => diComplex.hasNamedChild(nqn))
      case noSibling: InfosetNoNextSiblingException =>
        // If diSimple is an array occurrence, another occurrence being
        // appended to that same array is what resolves this the common
        // way, and only the array's own waiter is notified for that;
        // parent's waiter would only fire for a scalar child, or if the
        // parent itself later gains a child after diSimple's array.
        val diSimple = noSibling.diSimple
        val parent = diSimple.diParent
        val maybeArray = diSimple.maybeArray
        val waiter =
          if (maybeArray.isDefined) maybeArray.get.suspensionWaiter
          else parent.suspensionWaiter
        val subTarget: Maybe[AnyRef] = One(diSimple)
        if (!isAlreadyRegisteredOn(waiter, subTarget))
          reregister(waiter, subTarget, () => parent.hasNextSibling(diSimple))
      case noIndex: InfosetArrayIndexOutOfBoundsException =>
        // The index is available once length reaches it, not only once
        // length exceeds it. diArray's waiter is shared by whatever other
        // index a different retry of this same suspension might have
        // blocked on before, so index must also match.
        val diArray = noIndex.diArray
        val index = noIndex.index
        val waiter = diArray.suspensionWaiter
        val subTarget: Maybe[AnyRef] = One(java.lang.Long.valueOf(index))
        if (!isAlreadyRegisteredOn(waiter, subTarget))
          reregister(waiter, subTarget, () => diArray.length >= index)
      case _ =>
        // no targeted wake-up available for any other blocking reason
        clearAllRegistrations()
        maybeRegisteredSubTarget = Nope
    }

    // May also be blocked on specific DOSs' absolute positions. registerFor
    // is idempotent per-DOS, so this runs unconditionally rather than only
    // when the waiter changed: a stale extra registration is harmless,
    // since the condition re-verifies on notify either way.
    exc match {
      case noLength: InfosetLengthUnknownException =>
        val absBitPosDoses = noLength.lengthState.maybeAbsBitPosDoses
        if (absBitPosDoses.nonEmpty) {
          absBitPosDoses.foreach(dosListeners.registerFor)
        }
      case _ =>
    }
  }

  final def block(nodeOrVar: AnyRef, info: AnyRef, index: Long, exc: AnyRef): Unit = {
    Logger.log.debug(s"blocking ${this} due to ${exc}")

    // maybeRegisterWaiterFor, below, reconciles the registration
    // against what this block needs, touching nothing when a re-block
    // on the same reason is already correctly registered.
    Assert.usage(nodeOrVar ne null)
    Assert.usage(info ne null)
    Assert.usage(exc ne null)
    priorNodeOrVar = maybeNodeOrVar
    priorInfo = maybeInfo
    priorIndex = maybeIndex
    priorExc = maybeExc
    maybeNodeOrVar = One(nodeOrVar)
    maybeInfo = One(info)
    maybeIndex = MaybeInt(index.toInt)
    maybeExc = One(exc)
    done_ = false
    isBlocked_ = true

    if (isBlockedSameLocation) {
      isMakingProgress_ = false
    } else if (isBlockedFirstTime) {
      isMakingProgress_ = true
    } else {
      isMakingProgress_ = true
    }

    maybeRegisterWaiterFor(exc)
  }

  final def blockedLocation = "BLOCKED\nexc=%s\nnode=%s\ninfo=%s\nindex=%s".format(
    maybeExc,
    maybeNodeOrVar,
    maybeInfo,
    maybeIndex
  )

  private def isBlockedFirstTime: Boolean = {
    isBlocked &&
    priorNodeOrVar.isEmpty
  }

  private def isBlockedSameLocation: Boolean = {
    val res = isBlocked && {
      if (priorNodeOrVar.isEmpty) false
      else {
        Assert.invariant(maybeNodeOrVar.isDefined)
        val res =
          maybeNodeOrVar.get == priorNodeOrVar.get &&
            maybeInfo.get == priorInfo.get &&
            maybeIndex.get == priorIndex.get &&
            maybeExc.get == priorExc.get
        res
      }
    }
    res
  }

}
