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

package org.apache.daffodil.io

import java.io.ByteArrayInputStream
import java.io.File
import java.io.InputStreamReader

import org.apache.daffodil.core.util.TestUtils.intercept
import org.apache.daffodil.lib.exceptions.Abort
import org.apache.daffodil.lib.schema.annotation.props.gen.BitOrder
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.Suspension
import org.apache.daffodil.runtime1.processors.SuspensionWaiter
import org.apache.daffodil.runtime1.processors.unparsers.UState

import org.apache.commons.io.IOUtils
import org.junit.Assert.*
import org.junit.Test
import passera.unsigned.ULong

class TestDirectOrBufferedDataOutputStream {

  private def getString(baos: ByteArrayOrFileOutputStream) = {
    val is = new ByteArrayInputStream(baos.getBuf)
    val ir = new InputStreamReader(is, "ascii")
    val line = IOUtils.toString(ir)
    val res = line.replace("\u0000", "")
    res
  }

  def newDirectOrBufferedDataOutputStream(
    jos: java.io.OutputStream,
    creator: DirectOrBufferedDataOutputStream,
    bo: BitOrder = BitOrder.MostSignificantBitFirst
  ) = {
    val os = DirectOrBufferedDataOutputStream(
      jos,
      creator,
      false,
      4096,
      2000 * (1 << 20),
      new File("."),
      Maybe.Nope
    )
    os.setPriorBitOrder(bo)
    os
  }

  /**
   * Tests that the toString method doesn't throw. Can't even use a debugger
   * if that happens.
   */
  @Test def testToStringDoesNotThrow(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    assertFalse(layered.toString().isEmpty())
  }

  val finfo = FormatInfoForUnitTest()

  @Test def testCollapsingBufferIntoDirect1(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)

    layered.setFinished(finfo) // collapses layered into buf1.

    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

    assertTrue(buf1.isDirect)
    assertFalse(buf1.isFinished)

  }

  @Test def testCollapsingFinishedBufferIntoLayered(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)

    buf1.setFinished(finfo)

    assertTrue(buf1.isFinished)

    layered.setFinished(finfo) // collapses layered into buf1.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1", getString(baos))

  }

  @Test def testCollapsingTwoBuffersIntoDirect(): Unit = {

    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)

    val hw = "Hello World!"
    val hwBytes = hw.getBytes("ascii")

    layered.putBytes(hwBytes, finfo)

    assertEquals(hw, getString(baos))

    val buf1 = layered.addBuffered()
    val buf2 = buf1.addBuffered()

    buf1.putBytes("buf1".getBytes("ascii"), finfo)
    buf2.putBytes("buf2".getBytes("ascii"), finfo)

    assertTrue(buf2.isBuffering)

    buf1.setFinished(finfo) // buf1 finished while layered (before it) is still unfinished.

    assertTrue(buf1.isFinished)

    layered.setFinished(
      finfo
    ) // collapses layered into buf1. Since buf1 is finished already, this melds them, outputs everything
    // and leaves the whole thing finished.
    // leaves layered dead/unusable.

    assertTrue(buf1.isDead) // because it was finished when layered was subsequently finished
    assertTrue(layered.isDead)

    assertEquals(hw + "buf1" + "buf2", getString(baos))

    assertTrue(buf2.isDirect)

  }

  /**
   * A direct DOS that merges forward into a following buffered DOS goes
   * straight to dead, never through finished (see setFinished's isDirect
   * branch), but its own position/content is just as permanent at that
   * point, so anyone waiting on it via registerFinishedListener must
   * still be notified.
   */
  @Test def testCollapsingDirectIntoBufferedNotifiesFinishedListeners(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    layered.putBytes("Hello World!".getBytes("ascii"), finfo)

    val buf1 = layered.addBuffered()
    buf1.putBytes("buf1".getBytes("ascii"), finfo)

    var notified = false
    val waiter = new SuspensionWaiter {
      override def notifySuspensions(): Unit = notified = true
    }
    layered.registerFinishedListener(waiter)

    layered.setFinished(finfo) // collapses layered into buf1; layered goes dead, not finished.

    assertTrue(layered.isDead)
    assertFalse(layered.isFinished)
    assertTrue(layered.isFinishedOrDead)
    assertTrue(
      "expected the SuspensionWaiter registered on the direct DOS to be notified " +
        "once it merged away, even though it never reached the finished state",
      notified
    )
  }

  /**
   * Same as above, but for the other isDirect retirement path: nothing
   * following, so the direct DOS is flushed, closed, and marked dead at
   * the very end of the whole unparse instead of merging into a successor.
   */
  @Test def testFinishingDirectWithNoFollowingNotifiesFinishedListeners(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    layered.putBytes("Hello World!".getBytes("ascii"), finfo)

    var notified = false
    val waiter = new SuspensionWaiter {
      override def notifySuspensions(): Unit = notified = true
    }
    layered.registerFinishedListener(waiter)

    layered.setFinished(
      finfo
    ) // no following stream, so this is the final flush-and-close path.

    assertTrue(layered.isDead)
    assertFalse(layered.isFinished)
    assertTrue(layered.isFinishedOrDead)
    assertTrue(
      "expected the SuspensionWaiter registered on the direct DOS to be notified " +
        "once it was flushed and closed at the end, even though it never reached " +
        "the finished state",
      notified
    )
  }

  // A minimal double, same idiom as TestSuspensionTracker/LengthStateWaiterTest:
  // registerFor/removeSuspension only ever compare by reference identity and
  // never call doTask/rd, so no real UState is needed to prove registration
  // and deregistration happen. A real notify does go on to call
  // moveFromParkedToYoung, which requires a real savedUstate this double
  // doesn't have - that failure is expected and asserted for below, after
  // confirming the deregistration it's supposed to trigger already happened.
  private def newSuspension(): Suspension = new Suspension {
    override def rd = throw new NotImplementedError("not used by this test")
    override protected def doTask(ustate: UState): Unit = ()
  }

  /**
   * A newly buffered DOS starts with no absolute position
   * (addBufferedDOS calls resetAllBitPos on it). A suspension registered
   * on its settledWaiter must be notified (and deregistered) the
   * moment setAbsStartingBitPos0b first makes maybeAbsBitPos0b defined,
   * well before (and independent of) this DOS ever finishing.
   */
  @Test def testSetAbsStartingBitPos0bNotifiesSettledWaiter(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    val buf1 = layered.addBuffered()

    assertTrue(buf1.maybeAbsBitPos0b.isEmpty)

    val suspension = newSuspension()
    suspension.additionalWaiters.registerFor(buf1.settledWaiter)
    assertTrue(buf1.settledWaiter.isRegisteredSuspension(suspension))

    // The registered suspension has no real UState, so the notify this
    // triggers fails once it reaches moveFromParkedToYoung's own
    // savedUstate access - expected here, since what this test actually
    // verifies is that the notify fired at all, not the full retry.
    intercept[Abort] { buf1.setAbsStartingBitPos0b(ULong(42)) }

    assertTrue(buf1.maybeAbsBitPos0b.isDefined)
    assertFalse(buf1.isFinished)
    assertFalse(
      "expected the suspension to be deregistered as soon as the absolute " +
        "position became known, without waiting for buf1 to finish",
      buf1.settledWaiter.isRegisteredSuspension(suspension)
    )
  }

  /**
   * setNonZeroLength (called by anything that writes 1+ bits to the DOS)
   * is the eager Unknown -> NonZero transition. A suspension registered
   * beforehand must be notified (and deregistered) the moment that write
   * happens.
   */
  @Test def testWritingNotifiesSettledWaiterAsNonZero(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    val buf1 = layered.addBuffered()

    assertTrue(buf1.zeroLengthStatus eq ZeroLengthStatus.Unknown)

    val suspension = newSuspension()
    suspension.additionalWaiters.registerFor(buf1.settledWaiter)
    assertTrue(buf1.settledWaiter.isRegisteredSuspension(suspension))

    intercept[Abort] { buf1.putBytes("x".getBytes("ascii"), finfo) }

    assertTrue(buf1.zeroLengthStatus eq ZeroLengthStatus.NonZero)
    assertFalse(
      "expected the suspension to be deregistered as soon as a write made " +
        "the status NonZero",
      buf1.settledWaiter.isRegisteredSuspension(suspension)
    )
  }

  /**
   * Unlike NonZero, Unknown only ever resolves to Zero lazily, when
   * zeroLengthStatus is queried after the DOS finishes or dies having
   * had nothing written to it. setFinished must force that resolution
   * and notify, or a suspension waiting on a never-written DOS would
   * wait forever.
   */
  @Test def testFinishingWithNoWritesNotifiesSettledWaiterAsZero(): Unit = {
    val baos = new ByteArrayOrFileOutputStream(2000 * (1 << 20), new File("."), Maybe.Nope)
    val layered = newDirectOrBufferedDataOutputStream(baos, null)
    val buf1 = layered.addBuffered()

    assertTrue(buf1.zeroLengthStatus eq ZeroLengthStatus.Unknown)

    val suspension = newSuspension()
    suspension.additionalWaiters.registerFor(buf1.settledWaiter)
    assertTrue(buf1.settledWaiter.isRegisteredSuspension(suspension))

    intercept[Abort] { buf1.setFinished(finfo) }

    assertTrue(buf1.zeroLengthStatus eq ZeroLengthStatus.Zero)
    assertFalse(
      "expected the suspension to be deregistered once finishing " +
        "resolved the still-Unknown status to Zero",
      buf1.settledWaiter.isRegisteredSuspension(suspension)
    )
  }
}
