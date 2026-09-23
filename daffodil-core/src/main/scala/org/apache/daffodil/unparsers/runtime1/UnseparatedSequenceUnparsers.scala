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

import org.apache.daffodil.lib.exceptions.Assert
import org.apache.daffodil.lib.schema.annotation.props.gen.OccursCountKind
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.TermRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

trait Unseparated { self: SequenceChildUnparser =>

  def childProcessors = Vector(childUnparser)
}

class ScalarOrderedUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  trd: TermRuntimeData
) extends SequenceChildUnparser(childUnparser, srd, trd)
  with Unseparated {

  override protected def unparse(state: UState) = childUnparser.unparse1(state)
}

class RepOrderedUnseparatedSequenceChildUnparser(
  childUnparser: Unparser,
  srd: SequenceRuntimeData,
  erd: ElementRuntimeData
) extends RepeatingChildUnparser(childUnparser, srd, erd)
  with Unseparated {

  override def checkArrayPosAgainstMaxOccurs(state: UState): Boolean = {
    if (ock eq OccursCountKind.Implicit)
      state.arrayIterationPos <= maxRepeats(state)
    else
      true
  }
}

class OrderedUnseparatedSequenceUnparser(
  rd: SequenceRuntimeData,
  childUnparsers: Array[SequenceChildUnparser]
) extends OrderedSequenceUnparserBase(rd)
  with WriteUnparser {

  // Sequences of nothing (no initiator, no terminator, nothing at all) should
  // have been optimized away
  Assert.invariant(childUnparsers.length > 0)

  override val runtimeDependencies = Array()

  override def childProcessors = childUnparsers.toVector

  /**
   * Same shape as OrderedSeparatedSequenceUnparser's writeContent, minus
   * separator writing. Without this, ElementUnparserBase.writeContent's
   * WriteUnparser check would be false here, falling through to the
   * production unparse1 path against an untouched inputter.
   */
  override def writeContent(containerNode: DINode, state: UState): Unit = {
    val sharedCtx = state.sharedContext.get
    val complex = containerNode.asComplex

    var index = 0
    while (index < childUnparsers.length) {
      childUnparsers(index) match {
        case rep: RepeatingChildUnparser =>
          writeRepeatingTerm(rep, complex, sharedCtx, state)
        case cu =>
          writeRequiredTerm(cu, complex, sharedCtx, state)
      }
      index += 1
    }
  }

  /**
   * Writes an array/optional term's full occurrence loop, if it produced
   * any occurrences; nothing to do otherwise (no separator bookkeeping to
   * account for either way, unlike OrderedSeparatedSequenceUnparser's own
   * writeRepeatingTerm).
   */
  private def writeRepeatingTerm(
    rep: RepeatingChildUnparser,
    complex: DIComplex,
    sharedCtx: UnparseSharedContext,
    state: UState
  ): Unit = {
    val idx = state.childIndexStack.top.toInt
    if (sharedCtx.childExistsOrFinal(complex, idx)) {
      val next = complex.child(idx)
      if (next.erd eq rep.erd) {
        next match {
          case arrayNode: DIArray =>
            // Mirrors the separated sequence's identical push. See
            // its own comment.
            state.arrayIterationIndexStack.push(1L)
            state.occursIndexStack.push(1L)
            var arrayOcc = 0
            while (sharedCtx.childExistsOrFinal(arrayNode, arrayOcc)) {
              val occNode = sharedCtx.awaitChild(arrayNode, arrayOcc)
              rep.childUnparser
                .asInstanceOf[ElementUnparserBase]
                .writeContent(occNode, state)
              arrayNode.freeChildIfNoLongerNeeded(arrayOcc, state.releaseUnneededInfoset)
              arrayOcc += 1
              state.moveOverOneArrayIterationIndexOnly()
              state.moveOverOneOccursIndexOnly()
            }
            complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
            state.moveOverOneElementChildOnly()
            state.arrayIterationIndexStack.pop()
            state.occursIndexStack.pop()
          case scalarOptional =>
            val readyChild = sharedCtx.awaitChild(complex, idx)
            // Mirrors the separated sequence's identical pairing. See
            // its own comment for why occursIndexStack must track the
            // actual occurrence being written (dfdl:occursIndex() in
            // the occurrence's own content).
            state.arrayIterationIndexStack.push(1L)
            state.occursIndexStack.push(1L)
            rep.childUnparser
              .asInstanceOf[ElementUnparserBase]
              .writeContent(readyChild, state)
            state.moveOverOneElementChildOnly()
            complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
            state.moveOverOneArrayIterationIndexOnly()
            state.moveOverOneOccursIndexOnly()
            state.arrayIterationIndexStack.pop()
            state.occursIndexStack.pop()
        }
      }
      // else: this term produced zero occurrences; a different term's
      // child appeared in this tree position instead, and there's no
      // separator bookkeeping to resolve for it here, unlike the
      // separated sequence's own zeroOccurrences.
    }
    // else: no more children will ever come; this optional/array term is
    // absent, again with nothing further to do about it here.
  }

  /**
   * Writes a required term: a simple or complex element, a nested bare
   * group, or a statement-only term with no tree child of its own.
   */
  private def writeRequiredTerm(
    cu: SequenceChildUnparser,
    complex: DIComplex,
    sharedCtx: UnparseSharedContext,
    state: UState
  ): Unit = {
    cu.childUnparser match {
      // A plain scalar element term: await its one tree child, write
      // its content, then advance the element-child position.
      case elemUnp: ElementUnparserBase =>
        val idx = state.childIndexStack.top.toInt
        val child = sharedCtx.awaitChild(complex, idx)
        elemUnp.writeContent(child, state)
        state.moveOverOneElementChildOnly()
        complex.freeChildIfNoLongerNeeded(idx, state.releaseUnneededInfoset)
      case wu: WriteUnparser =>
        val idx = state.childIndexStack.top.toInt
        // This nested group (e.g. a choice) may have resolved to a branch
        // with no infoset footprint at all; only wait for a child's
        // readiness when childExistsOrFinal says one genuinely exists.
        if (sharedCtx.childExistsOrFinal(complex, idx)) {
          sharedCtx.awaitChild(complex, idx)
        }
        wu.writeContent(complex, state)
      case nvi: NewVariableInstanceEndUnparser =>
        // Always runs, with no isBuildOnly gate: build's own recursion
        // already popped BuildState's local NVI scope stack; write's call
        // performs the polymorphically-different pop of the shared vTable,
        // which only write's call ever does.
        nvi.unparse1(state)
      case align: AlignmentPrimUnparser =>
        // Padding has no non-idempotent side effect (unlike setVariable/assert
        // below), so re-running it here is required, not forbidden: build's
        // own recursion only ran it against build's no-op-sink DOS.
        align.unparse1(state)
      case _ =>
        // Build's own unconditional recursion already ran this term exactly
        // once; running it again would re-evaluate a
        // dfdl:setVariable/assert/discriminator expression (e.g. "cannot
        // set variable twice").
        ()
    }
  }

  /**
   * Unparses one iteration of an array/optional element
   */
  inline protected def unparseOne(
    unparser: SequenceChildUnparser,
    trd: TermRuntimeData,
    state: UState
  ): Unit = {

    unparser.unparse1(state)
  }

  /**
   * Unparses an entire sequence, including both scalar and array/optional children.
   */
  protected def unparse(state: UState): Unit = {

    state.groupIndexStack.push(1L) // one-based indexing

    var index = 0
    var doUnparser = false
    val limit = childUnparsers.length
    while (index < limit) {
      val childUnparser = childUnparsers(index)
      val trd = childUnparser.trd
      state.pushTRD(trd) // because we inspect before we invoke the unparser

      //
      // Unparsing an ordered sequence depends on the incoming
      // stream of infoset events matching up with the order that
      // they are expected as the unparser recurses through the
      // child term unparsers.
      //
      childUnparser match {
        case unparser: RepeatingChildUnparser => {
          state.arrayIterationIndexStack.push(1L)
          state.occursIndexStack.push(1L)
          val erd = unparser.erd
          var numOccurrences = 0
          val maxReps = unparser.maxRepeats(state)

          //
          // The number of occurrances we unparse is always exactly driven
          // by the number of infoset events for the repeating/optional element.
          //
          // For RepUnparser - array/optional case - in all cases we should get a
          // startArray event. If we don't then
          // the element must be entirely optional, so we get no events for it
          // at all.

          // we must have an event
          Assert.invariant(state.inspect, "No event for unparing.")

          val ev = state.inspectAccessor
          if (ev.erd eq erd) {
            // must be a start event for this array/optional unparser
            val isArr = ev.isArray
            Assert.invariant(ev.isStart && (isArr || ev.erd.isOptional))

            //
            // StartArray for this unparser's array element
            //
            unparser.startArrayOrOptional(state)
            while ({
              doUnparser = unparser.shouldDoUnparser(unparser, state)
              doUnparser
            }) {
              if (isArr)
                if (state.dataProc.isDefined)
                  state.dataProc.get.beforeRepetition(state, this)

              unparseOne(unparser, erd, state)
              numOccurrences += 1
              state.moveOverOneArrayIterationIndexOnly()
              state.moveOverOneOccursIndexOnly()
              state.moveOverOneGroupIndexOnly() // array elements are always represented.

              if (isArr)
                if (state.dataProc.isDefined)
                  state.dataProc.get.afterRepetition(state, this)
            }

            // DAFFODIL-115: if the number of occurrences is less than minOccurs we are supposed
            // to check if the element is defaultable and add elements until we reach that
            // number. Daffodil does not currently support defaulting during unparsing.

            unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              unparser,
              numOccurrences,
              maxReps,
              state.arrayIterationPos - 1
            )
            unparser.endArrayOrOptional(erd, state)
          } else {
            // this is either a start event for a following element or an end event for a
            // parent element. Either way, we never saw a start event for this array/optional,
            // which means there were zero occurrenes. Make sure that is allowed for this array
            //
            // DAFFODIL-115: if the number of occurrences is less than minOccurs we are supposed
            // to check if the element is defaultable and add elements until we reach that
            // number. Daffodil does not currently support defaulting during unparsing.
            unparser.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              unparser,
              numOccurrences,
              maxReps,
              0
            )
          }

          state.arrayIterationIndexStack.pop()
          state.occursIndexStack.pop()
        }
        //
        case scalarUnparser => {
          unparseOne(scalarUnparser, trd, state)
          // only move over in group if the scalar "thing" is an element
          // that is represented.
          scalarUnparser.trd match {
            case erd: ElementRuntimeData if (!erd.isRepresented) => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }

      state.popTRD(trd)
      index += 1
      //
      // Note: the invariant is that unparsers move over 1 within their group themselves
      // we do not do the moving over here as we are the caller of the unparser.
      //
    }

    state.groupIndexStack.pop()
    //
    // this is establishing the invariant that unparsers (in this case the sequence unparser)
    // moves over within its containing group. The caller of an unparser does not do this move.
    //
    // state.moveOverOneGroupIndexOnly() // move past this sequence itself, next group child it ITS parent.
  }

}
