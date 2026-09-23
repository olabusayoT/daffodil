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

import org.apache.daffodil.lib.util.Coroutine
import org.apache.daffodil.lib.util.MainCoroutine

/*
 * The build/write handoff for build/write-prefetch: build and write's
 * own content dispatch hand off via Coroutine[T], guaranteeing only one
 * of the pair ever runs at a time, via a blocking-queue rendezvous
 * between two threads.
 */

/** Signals build sends to write when resuming it. */
sealed trait BuildSignal

/** Build made progress since write last ran; try again. */
case object MoreTreeAvailable extends BuildSignal

/**
 * Build's top-level recursion has fully returned; no further nodes will
 * ever be added. Sent exactly once, as build's final handoff; possibly
 * as the very FIRST signal write's coroutine ever receives, if build's
 * whole recursion never crossed the prefetch-lead threshold (small
 * enough document that write's thread was never spawned before build
 * finished). Write's loop must check for this on the result of ANY
 * resume, including the one that started its thread, not just as a
 * distinct "later" case.
 */
case object BuildFinished extends BuildSignal

/**
 * Build's own thread failed with an exception before ever reaching its
 * normal `BuildFinished` handoff (anywhere inside build's own top-level
 * recursion, or the invariant checks immediately around it). Write's
 * thread is guaranteed to be parked at this exact moment (build and
 * write never run concurrently), so this is how build's own exception
 * handling wakes it back up instead of leaving it blocked forever.
 * Write's own normal finalization assumes a consistently, fully-built
 * tree that a genuine abort may not have produced, so this signal skips
 * that path entirely; build's own exception, not anything from write's
 * side, is what actually gets reported to the caller.
 */
case object BuildAborted extends BuildSignal

/** Signals write sends to build when yielding control back. */
sealed trait WriteSignal

/**
 * Write's own recursive dispatch has blocked, needing more tree before
 * it can continue; build should keep building (and will be resumed
 * again itself once it does, or once build's own recursion finishes and
 * sends `BuildFinished` instead).
 */
case object WriteNeedsMore extends WriteSignal

/**
 * Write has finished all remaining work, or failed trying to. Sent
 * exactly once, as write's last act: call `resumeFinal`, then return
 * from `run()` immediately (its own contract). `error` is the captured
 * exception if write's work failed, so it can be re-thrown on build's
 * thread and handled uniformly with an exception thrown directly there.
 */
final case class WriteDone(error: Option[Throwable]) extends WriteSignal

/**
 * Build's side of the handoff. Runs on the ORIGINAL calling thread
 * (`isMain = true`, so no thread is spawned for it); build already drives
 * everything from the caller's thread; only write gets a genuinely new
 * thread (`WriteCoroutine` below).
 */
final class BuildCoroutine extends MainCoroutine[WriteSignal]

/**
 * Write's side of the handoff. An actual, separate thread, spawned lazily
 * (via `Coroutine`'s own `init()`) the first time build resumes it.
 * `runBody` (the caller-supplied build/write driver) must construct
 * write's own `UState` as its FIRST action on this thread, not before it
 * starts and not on the build thread, since `DataOutputStream` is
 * single-thread-affine; constructing it eagerly on build's thread would
 * bind it to the wrong one.
 *
 * `runBody` receives the signal that started this thread as its second
 * argument, rather than `run()` silently discarding it, because that
 * first signal might already be `BuildFinished`; `runBody` must treat it
 * like any later resume result, not assume the first is always
 * `MoreTreeAvailable`. It must end by calling
 * `resumeFinal(buildCoroutine, WriteDone(...))` (call it, then return
 * from `run()` immediately; its own contract).
 */
final class WriteCoroutine(runBody: (WriteCoroutine, BuildSignal) => Unit)
  extends Coroutine[BuildSignal] {
  override protected def run(): Unit = {
    val firstSignal = waitForResume()
    runBody(this, firstSignal)
  }
}
