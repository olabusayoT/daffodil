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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.lib.util.Maybe.One
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEndEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchEvent
import org.apache.daffodil.runtime1.infoset.ChoiceBranchStartEvent
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.ModelGroupRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.unparsers.runtime1.RepeatingChildUnparser
import org.apache.daffodil.unparsers.runtime1.SequenceChildUnparser

/**
 * A node in a much smaller, dedicated tree of Builders that parallels the
 * full Unparser tree: only Grams that actually create or select infoset
 * content (elements, sequences, choices, hidden groups) contribute one, so
 * building the infoset never has to dispatch through the many write-only
 * wrapper unparsers (delimiters, escape schemes, layers, padding,
 * specified-length) that sit between them in the Unparser tree.
 *
 * A Builder is immutable compiled-schema state shared by every parse. The
 * per-parse position lives in the BuildFrame it creates, on a BuildCursor's
 * explicit stack, so building can stop after any step and continue later
 * without holding a thread or a JVM call stack.
 */
trait Builder extends Serializable {
  def newFrame(): BuildFrame
}

/**
 * One Builder's in-progress state for one parse. `step` performs one
 * transition and must either push exactly one child frame onto the cursor
 * (this frame is stepped again once that child pops) or pop itself from the
 * cursor to signal it is complete.
 */
abstract class BuildFrame {
  def step(cursor: BuildCursor): Unit
}

/**
 * The explicit stack of BuildFrames that stands in for the call stack of a
 * recursive build. `advance` runs it until the lead window is full, so a
 * caller that needs more infoset tree can pull it forward directly. Driven
 * against a `BuildState`, never a write-side `UState`.
 */
final class BuildCursor(root: Builder, val state: UState, ctx: UnparseSharedContext) {
  private var stack = new Array[BuildFrame](32)
  private var depth = 0
  private var failure: Throwable = null

  push(root.newFrame())

  def push(frame: BuildFrame): Unit = {
    if (depth == stack.length) {
      stack = java.util.Arrays.copyOf(stack, depth * 2)
    }
    stack(depth) = frame
    depth += 1
  }

  def pop(): Unit = {
    depth -= 1
    stack(depth) = null
  }

  def isFinished: Boolean = depth == 0

  /**
   * Steps until at least one more node is built and either the lead window
   * is full or too many suspensions are pending, or until building
   * completes. A failure is rethrown, here and on every later call, as a
   * BuildAbortedException wrapping the original.
   */
  def advance(): Unit = {
    if (failure != null) {
      throw new BuildAbortedException(failure)
    }
    var lead = ctx.currentLead
    try {
      while (depth > 0) {
        stack(depth - 1).step(this)
        // Only a step that added a node can newly hit either limit: the
        // lead only grows by node additions, and build never adds
        // suspensions, so pending suspensions can only shrink here.
        val newLead = ctx.currentLead
        if (newLead != lead) {
          lead = newLead
          if (
            ctx.leadExceedsPrefetchLimit ||
            ctx.suspensionTracker.pendingCount > ctx.pendingSuspensionTripLimit
          ) {
            return
          }
        }
      }
    } catch {
      case t: Throwable => {
        failure = t
        depth = 0
        throw new BuildAbortedException(t)
      }
    }
  }

  def runToCompletion(): Unit = {
    while (!isFinished) {
      advance()
    }
  }
}

/**
 * A no-op stand-in for a choice branch or sequence child whose content is
 * empty (e.g. an empty sequence), so its build-time presence can still be
 * recorded without anything actually needing to happen.
 */
object EmptyBuilder extends Builder {
  private object EmptyFrame extends BuildFrame {
    override def step(cursor: BuildCursor): Unit = cursor.pop()
  }
  override def newFrame(): BuildFrame = EmptyFrame
}

/**
 * Builds each of several sibling Grams' content in order. Used only where a
 * `~` composition has more than one child that actually builds infoset
 * content; the common case of at most one such child never needs this.
 */
final class SeqCompBuilder(children: Array[Builder]) extends Builder {
  override def newFrame(): BuildFrame = new BuildFrame {
    private var i = 0
    override def step(cursor: BuildCursor): Unit = {
      if (i < children.length) {
        val child = children(i)
        i += 1
        cursor.push(child.newFrame())
      } else {
        cursor.pop()
      }
    }
  }
}

/**
 * Builds one element's infoset node and, for complex types, builds
 * descendant nodes via contentBuilder. unparseBegin/unparseEnd are the
 * same element-kind-specific (plain/nillable/OVC/etc.) node-creation logic
 * unparse() itself uses, including the bounded-lookahead lead-counter
 * hookup and the deferred simple-value finalization; only the "what does
 * this element contain" step is redirected to the builder tree instead
 * of back into the unparser tree.
 */
final class ElementBuilder(
  erd: ElementRuntimeData,
  unparseBegin: UState => Unit,
  unparseEnd: UState => Unit,
  contentBuilder: Maybe[Builder]
) extends Builder {

  override def newFrame(): BuildFrame = new BuildFrame {
    private var contentPushed = false

    override def step(cursor: BuildCursor): Unit = {
      val state = cursor.state
      if (!contentPushed) {
        unparseBegin(state)
        if (erd.isComplexType) {
          state.pushTRD(erd.optComplexTypeModelGroupRuntimeData.get)
          if (contentBuilder.isDefined) {
            contentPushed = true
            cursor.push(contentBuilder.get.newFrame())
            return
          }
        }
      }
      if (erd.isComplexType) {
        state.popTRD(erd.optComplexTypeModelGroupRuntimeData.get)
      }
      unparseEnd(state)
      cursor.pop()
    }
  }
}

/**
 * Pairs a sequence child's existing occurs-count/array bookkeeping (reused
 * as-is from the Unparser tree, since it is cheap, pure state bookkeeping
 * unrelated to the tree-walking overhead this Builder tree exists to avoid)
 * with that same child's own Builder, which SequenceBuilder builds
 * instead of the child's full Unparser.
 */
final case class SequenceChildBuildInfo(
  childUnparser: SequenceChildUnparser,
  childBuilder: Builder
)

/**
 * Builds an entire sequence's children, scalar and array/optional alike,
 * each through its own Builder.
 */
final class SequenceBuilder(children: Array[SequenceChildBuildInfo]) extends Builder {

  override def newFrame(): BuildFrame = new SequenceFrame

  private final class SequenceFrame extends BuildFrame {
    // NextChild: between children. AfterScalar/AfterOccurrence: a child's
    // frame just popped. InArray: array/optional loop is between occurrences.
    private final val NextChild = 0
    private final val AfterScalar = 1
    private final val InArray = 2
    private final val AfterOccurrence = 3

    private var phase = NextChild
    private var started = false
    private var index = 0
    // children(index), read once per child and used by every later phase.
    private var current: SequenceChildBuildInfo = null
    private var rep: RepeatingChildUnparser = null
    private var numOccurrences = 0
    private var maxReps = 0L

    override def step(cursor: BuildCursor): Unit = {
      val state = cursor.state
      if (!started) {
        started = true
        state.groupIndexStack.push(1L)
      }
      phase match {
        case NextChild => nextChild(cursor, state)
        case AfterScalar => {
          current.childUnparser.trd match {
            case erd: ElementRuntimeData if !erd.isRepresented => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
          finishChild(state)
        }
        case AfterOccurrence => {
          numOccurrences += 1
          state.moveOverOneArrayIterationIndexOnly()
          state.moveOverOneOccursIndexOnly()
          state.moveOverOneGroupIndexOnly()
          phase = InArray
        }
        case InArray => {
          if (rep.shouldDoUnparser(rep, state)) {
            phase = AfterOccurrence
            cursor.push(current.childBuilder.newFrame())
          } else {
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              state.arrayIterationPos - 1
            )
            rep.endArrayOrOptional(rep.erd, state)
            finishRepeating(state)
          }
        }
      }
    }

    private def nextChild(cursor: BuildCursor, state: UState): Unit = {
      if (index == children.length) {
        state.groupIndexStack.pop()
        cursor.pop()
      } else {
        current = children(index)
        val cu = current.childUnparser
        state.pushTRD(cu.trd)
        cu match {
          case r: RepeatingChildUnparser => {
            rep = r
            state.arrayIterationIndexStack.push(1L)
            state.occursIndexStack.push(1L)
            numOccurrences = 0
            maxReps = r.maxRepeats(state)

            Assert.invariant(state.inspect, "No event for building.")
            val ev = state.inspectAccessor
            if (ev.erd eq r.erd) {
              r.startArrayOrOptional(state)
              phase = InArray
            } else {
              r.checkFinalOccursCountBetweenMinAndMaxOccurs(
                state,
                r,
                numOccurrences,
                maxReps,
                0
              )
              finishRepeating(state)
            }
          }
          case _ => {
            phase = AfterScalar
            cursor.push(current.childBuilder.newFrame())
          }
        }
      }
    }

    private def finishRepeating(state: UState): Unit = {
      state.arrayIterationIndexStack.pop()
      state.occursIndexStack.pop()
      rep = null
      finishChild(state)
    }

    private def finishChild(state: UState): Unit = {
      state.popTRD(children(index).childUnparser.trd)
      index += 1
      phase = NextChild
    }
  }
}

/**
 * Builds just the one structurally-present branch of a choice, resolved
 * from the next infoset event, through that branch's own Builder.
 */
final class ChoiceBuilder(
  mgrd: ModelGroupRuntimeData,
  branchMap: Map[ChoiceBranchEvent, (TermRuntimeData, Builder)],
  defaultBranch: Maybe[(TermRuntimeData, Builder)]
) extends Builder {

  private def resolveBranch(state: UState): (TermRuntimeData, Builder) = {
    if (state.withinHiddenNest) {
      defaultBranch.get
    } else {
      state.pushTRD(mgrd)
      val event = state.inspectOrError
      val key: ChoiceBranchEvent = event match {
        case e if e.isStart && (e.isElement || e.isArray) =>
          ChoiceBranchStartEvent(e.erd.namedQName)
        case e if e.isEnd && (e.isElement || e.isArray) =>
          ChoiceBranchEndEvent(e.erd.namedQName)
      }
      val fromTable = branchMap.get(key)
      val resolved = if (fromTable.isDefined) {
        fromTable
      } else {
        defaultBranch.toOption
      }
      if (resolved.isEmpty) {
        UnparseError(
          One(mgrd.schemaFileLocation),
          One(state.currentLocation),
          "Found next element %s, but expected one of %s.",
          key.qname.toExtendedSyntax,
          branchMap.keys.map { _.qname.toExtendedSyntax }.mkString(", ")
        )
      }
      state.popTRD(mgrd)
      resolved.get
    }
  }

  override def newFrame(): BuildFrame = new BuildFrame {
    private var branchTRD: TermRuntimeData = null

    override def step(cursor: BuildCursor): Unit = {
      val state = cursor.state
      if (branchTRD == null) {
        val (trd, builder) = resolveBranch(state)
        branchTRD = trd
        state.pushTRD(trd)
        cursor.push(builder.newFrame())
      } else {
        state.popTRD(branchTRD)
        cursor.pop()
      }
    }
  }
}

/**
 * Builds the body of a hidden group. withinHiddenNest must stay maintained
 * during build too: it is what tells a hidden element's unparseBegin/
 * unparseEnd to manufacture a node instead of consuming an event that will
 * never exist.
 */
final class HiddenGroupBuilder(bodyBuilder: Builder) extends Builder {
  override def newFrame(): BuildFrame = new BuildFrame {
    private var bodyPushed = false

    override def step(cursor: BuildCursor): Unit = {
      if (!bodyPushed) {
        bodyPushed = true
        cursor.state.incrementHiddenDef()
        cursor.push(bodyBuilder.newFrame())
      } else {
        cursor.state.decrementHiddenDef()
        cursor.pop()
      }
    }
  }
}

/**
 * A nilled complex element has no children to build; nilled-ness is only
 * known once the node exists, so this checks it at build time rather than
 * resolving statically to either branch.
 */
final class NilOrContentBuilder(contentBuilder: Builder) extends Builder {
  override def newFrame(): BuildFrame = new BuildFrame {
    private var contentPushed = false

    override def step(cursor: BuildCursor): Unit = {
      if (!contentPushed && !cursor.state.currentInfosetNode.asComplex.isNilled) {
        contentPushed = true
        cursor.push(contentBuilder.newFrame())
      } else {
        cursor.pop()
      }
    }
  }
}
