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
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.util.Logger
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.*
import org.apache.daffodil.lib.util.MaybeInt
import org.apache.daffodil.lib.util.MaybeULong
import org.apache.daffodil.runtime1.infoset.InfosetLengthUnknownException
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
trait Suspension extends Serializable {

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
   * SuspensionTracker is the only caller - see moveFromParkedToYoung.
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

  private def prepareToSuspend(ustate: UState): Unit = {
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
    // TODO: Performance - a copy-on-write scheme could avoid that copy.
    //
    val didSplit = (ustate.getDataOutputStream ne original)
    val cloneUState = ustate.asInstanceOf[UStateMain].cloneForSuspension(original)
    if (isReadOnly && didSplit) {
      Assert.invariantFailed("Shouldn't have split. read-only case")
    }

    savedUstate_ = cloneUState

    val mainUState = ustate.asInstanceOf[UStateMain]
    maybeTracker = One(mainUState.suspensionTracker)
    mainUState.addSuspension(this)
  }

  // Stashed at suspend() time so moveFromParkedToYoung can find its
  // tracker later. Always defined by then: a suspension only becomes a
  // LengthState waiter after its first blocked attempt, which suspend()
  // always follows immediately.
  private var maybeTracker: Maybe[SuspensionTracker] = Nope

  /**
   * Called by a SuspensionWaiter's notifySuspensions once it's confirmed
   * this suspension is worth a real attempt again: hands it to its
   * tracker for a real attempt (SuspensionTracker is the only caller of
   * runSuspension), clearing isWaitingOnWaiter so it isn't immediately
   * re-parked first.
   */
  final def moveFromParkedToYoung(): Unit = {
    Assert.invariant(maybeTracker.isDefined)
    Assert.invariant(maybeRegisteredWaiter.isDefined)
    maybeRegisteredWaiter = Nope
    maybeTracker.get.moveParkedToYoung(this)
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
  }

  final def isDone = done_

  final def isBlocked = isBlocked_

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
  // block() unconditionally deregisters this before any retry, so
  // registerWaiter never finds one already set.
  private var maybeRegisteredWaiter: Maybe[SuspensionWaiter] = Nope

  /**
   * True exactly when a targeted wake-up is registered against some
   * SuspensionWaiter (set only from registerWaiter). SuspensionTracker's
   * periodic sweep uses this to skip re-running doTask until that
   * wake-up fires.
   */
  final def isWaitingOnWaiter: Boolean = maybeRegisteredWaiter.isDefined

  final def registerWaiter(w: SuspensionWaiter): Unit = {
    Assert.invariant(maybeRegisteredWaiter.isEmpty)
    maybeRegisteredWaiter = One(w)
    w.registerSuspension(this)
  }

  /**
   * Registers a targeted wake-up when exc (the reason this suspension
   * just blocked, via block() immediately beforehand) has a known
   * SuspensionWaiter to register with - a no-op otherwise. Shared by
   * DPath.scala's expression evaluation and SuspendableOperation's
   * retry loop.
   */
  final def maybeRegisterWaiterFor(exc: AnyRef): Unit = exc match {
    case noLength: InfosetLengthUnknownException =>
      registerWaiter(noLength.lengthState.suspensionWaiter)
    case noVar: VariableException =>
      registerWaiter(noVar.variableInstance.suspensionWaiter)
    case _ => // no targeted wake-up available for any other blocking reason
  }

  final def block(nodeOrVar: AnyRef, info: AnyRef, index: Long, exc: AnyRef): Unit = {
    Logger.log.debug(s"blocking ${this} due to ${exc}")

    // Deregister unconditionally; a re-block on the same reason
    // re-registers right after via maybeRegisterWaiterFor, so this is a
    // no-op then. For any other blocking reason, this suspension is no
    // longer waiting on it - leaving the registration would keep it
    // reachable, and isWaitingOnWaiter reads this same field.
    if (maybeRegisteredWaiter.isDefined) {
      maybeRegisteredWaiter.get.removeSuspension(this)
      maybeRegisteredWaiter = Nope
    }

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
