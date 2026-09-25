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

import java.lang.Boolean as JBoolean
import scala.collection.mutable.ArrayBuffer

import org.apache.daffodil.api
import org.apache.daffodil.api.Daffodil.InfosetInputterEventType
import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.exceptions.UnsuppressableException
import org.apache.daffodil.lib.util.Coroutine
import org.apache.daffodil.runtime1.dpath.NodeInfo
import org.apache.daffodil.runtime1.infoset.DIElement
import org.apache.daffodil.runtime1.infoset.DISimple
import org.apache.daffodil.runtime1.processors.unparsers.PrefetchEvent.End
import org.apache.daffodil.runtime1.processors.unparsers.PrefetchEvent.EndDocument
import org.apache.daffodil.runtime1.processors.unparsers.PrefetchEvent.Start
import org.apache.daffodil.runtime1.processors.unparsers.PrefetchEvent.StartDocument

/**
 * One event of the growing, build-side event log that
 * [[BuildWritePrefetchInputter]] replays for write, appended one at a time
 * as build produces it rather than captured all at once from a completed
 * tree.
 */
private[unparsers] sealed trait PrefetchEvent {
  def elementMaybe: Option[DIElement] = this match {
    case Start(e) => Some(e)
    case End(e) => Some(e)
    case StartDocument | EndDocument => None
  }
}

private[unparsers] object PrefetchEvent {
  case object StartDocument extends PrefetchEvent
  case object EndDocument extends PrefetchEvent
  final case class Start(elem: DIElement) extends PrefetchEvent
  final case class End(elem: DIElement) extends PrefetchEvent
}

/**
 * What the build coroutine hands back to the write coroutine when it resumes
 * it: either it has paused having filled its prefetch window (more will
 * become available on the next resume), or it has reached the true end of
 * the infoset, or it failed.
 */
private[unparsers] sealed trait BuildProgress
private[unparsers] object BuildProgress {
  case object WindowFilled extends BuildProgress
  case object Finished extends BuildProgress
  final case class Failed(cause: Throwable) extends BuildProgress
}

/**
 * The write side of the build/write-prefetch handoff. This runs on the
 * calling (main) thread: it is never itself given a thread to run on, only
 * resumed by the build coroutine.
 */
private[unparsers] final class WriteCoroutine extends Coroutine[BuildProgress] {
  override final def isMain = true
  // $COVERAGE-OFF$
  override protected def run(): Unit =
    Assert.invariantFailed(
      "WriteCoroutine.run() should never be called; it is the main coroutine."
    )
  // $COVERAGE-ON$
}

/**
 * The build side of the build/write-prefetch handoff. Runs `doBuild` on its
 * own thread, pausing (resuming the write coroutine) each time build's lead
 * over write reaches the configured prefetch window, and blocking there
 * until write resumes it to ask for more.
 */
private[unparsers] final class BuildCoroutine(
  writeCoroutine: WriteCoroutine,
  windowNodes: Int,
  doBuild: () => Unit
) extends Coroutine[Unit] {

  /** Number of elements fully built (End event emitted) so far. */
  private var builtCount: Int = 0

  /** Number of elements write has fully consumed (End event replayed) so far. */
  private var writtenCount: Int = 0

  private def lead: Int = builtCount - writtenCount

  /** The largest lead build ever reached, exposed only for tests to confirm
   * the prefetch window is actually being respected (never exceeded) and,
   * for a large enough infoset, actually being used (reached).
   */
  private var maxLeadSeen: Int = 0
  private[unparsers] def maxLeadObserved: Int = maxLeadSeen

  override protected def run(): Unit = {
    waitForResume()
    try {
      doBuild()
      resumeFinal(writeCoroutine, BuildProgress.Finished)
    } catch {
      case s: scala.util.control.ControlThrowable => throw s
      case u: UnsuppressableException => throw u
      case e: Throwable => resumeFinal(writeCoroutine, BuildProgress.Failed(e))
    }
  }

  /**
   * Called by build (from this coroutine's own thread) once an element has
   * finished being built. Pauses, handing control to write, once build's
   * lead over write exceeds the prefetch window; write resumes this
   * coroutine later to let build continue.
   */
  def elementBuilt(): Unit = {
    builtCount += 1
    if (lead > maxLeadSeen) { maxLeadSeen = lead }
    if (lead > windowNodes) {
      resume(writeCoroutine, BuildProgress.WindowFilled)
    }
  }

  /**
   * Called by write (from the write coroutine's thread, i.e. the caller of
   * resume/waitForResume below) once it has fully consumed an element,
   * shrinking build's lead back down.
   */
  def elementWritten(): Unit = {
    writtenCount += 1
  }
}

/**
 * Shared context wiring a BuildCoroutine/WriteCoroutine pair together for one
 * unparse call, and buffering the events build produces for write to replay.
 * There is no synchronization here beyond what Coroutine's own blocking
 * handoff already provides: build and write never run concurrently, so plain
 * mutable state read and written across a resume/waitForResume pair is safe.
 */
final class BuildWritePrefetchController(windowNodes: Int, doBuild: () => Unit) {

  private val writeCoroutine = new WriteCoroutine
  // Wraps the caller's build logic with the document-level start/end events,
  // so the caller doesn't need a reference to this controller (which
  // doesn't exist yet at the point this closure is built) just to emit them.
  private val buildCoroutine = new BuildCoroutine(
    writeCoroutine,
    windowNodes,
    () => {
      emitStartDocument()
      doBuild()
      emitEndDocument()
    }
  )

  private val events = new ArrayBuffer[PrefetchEvent]()
  private var buildDone = false
  private var buildFailure: Option[Throwable] = None

  /** Called by BuildWritePrefetchInputter once it needs an event beyond what
   * build has produced so far. This is also what starts build running in
   * the first place, the first time write needs anything at all. No-ops if
   * build has already finished.
   */
  private def requestMore(): Unit = {
    if (!buildDone) {
      writeCoroutine.resume(buildCoroutine, ()) match {
        case BuildProgress.WindowFilled => // build will produce more on the next requestMore()
        case BuildProgress.Finished => buildDone = true
        case BuildProgress.Failed(e) => {
          buildDone = true
          buildFailure = Some(e)
        }
      }
    }
  }

  private[unparsers] def eventAt(pos: Int): PrefetchEvent = events(pos)
  private[unparsers] def size: Int = events.length

  /** The largest lead build ever reached, exposed only for tests. */
  private[unparsers] def maxLeadObserved: Int = buildCoroutine.maxLeadObserved

  /**
   * Ensures at least `pos + 1` events have been produced, pumping build via
   * the coroutine handoff as needed. If build failed before producing that
   * many, re-throws its failure here so write's own unparse call stack
   * handles it exactly as it would one of its own failures.
   */
  private[unparsers] def ensureAvailable(pos: Int): Unit = {
    while (pos >= events.length && !buildDone) {
      requestMore()
    }
    if (pos >= events.length && buildFailure.isDefined) {
      throw buildFailure.get
    }
  }

  private def emitStartDocument(): Unit = events += StartDocument
  private def emitEndDocument(): Unit = events += EndDocument

  // Called only from build's own thread.
  def emitStart(elem: DIElement): Unit = {
    if (!elem.isHidden) { events += Start(elem) }
  }

  def emitEnd(elem: DIElement): Unit =
    if (!elem.isHidden) {
      events += End(elem)
      buildCoroutine.elementBuilt()
    }

  // Called only from write's own thread.
  private[unparsers] def consumed(event: PrefetchEvent): Unit = event match {
    case End(_) => buildCoroutine.elementWritten()
    case _ => // only a completed element (its End) frees up window room
  }
}

/**
 * Replays the growing, build-side event log a BuildWritePrefetchController
 * maintains as an InfosetInputter, so write consumes it through the same
 * event-driven unparse() code any other infoset source uses. Whenever write
 * needs an event build has not produced yet, it blocks (via the
 * controller's coroutine handoff) until build either produces it or reaches
 * the end of the infoset.
 */
final class BuildWritePrefetchInputter(
  controller: BuildWritePrefetchController
) extends api.infoset.InfosetInputter {

  private var pos = 0

  private def current: PrefetchEvent = {
    controller.ensureAvailable(pos)
    controller.eventAt(pos)
  }

  override def getEventType(): InfosetInputterEventType = current match {
    case StartDocument => InfosetInputterEventType.StartDocument
    case EndDocument => InfosetInputterEventType.EndDocument
    case Start(_) => InfosetInputterEventType.StartElement
    case End(_) => InfosetInputterEventType.EndElement
  }

  override def getLocalName(): String = current.elementMaybe.get.erd.namedQName.local

  override def getSupportsNamespaces = true

  override def getNamespaceURI(): String =
    current.elementMaybe.get.erd.namedQName.namespace.toStringOrNullIfNoNS

  override def getSimpleText(
    primType: NodeInfo.Kind,
    runtimeProperties: java.util.Map[String, String]
  ): String = {
    val simple = current.elementMaybe.get.asInstanceOf[DISimple]
    // Nilled elements, and any other valueless elements this replay reaches,
    // have nothing to stringify; the caller only uses this text when there's
    // actually a value to set (createElement checks isNilled/isOutputValueCalc
    // itself), so an empty string here is discarded either way.
    if (simple.hasValue) simple.dataValueAsString else ""
  }

  override def isNilled(): JBoolean = {
    val elem = current.elementMaybe.get
    if (!elem.erd.isNillable) null
    else JBoolean.valueOf(elem.isNilled)
  }

  override def hasNext(): Boolean = {
    controller.ensureAvailable(pos + 1)
    pos + 1 < controller.size
  }

  override def next(): Unit = {
    controller.consumed(current)
    pos += 1
  }

  override def fini(): Unit = ()
}
