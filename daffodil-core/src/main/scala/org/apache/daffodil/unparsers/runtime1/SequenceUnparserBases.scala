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
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

abstract class OrderedSequenceUnparserBase(
  srd: SequenceRuntimeData
) extends CombinatorUnparser(srd) {
  override def nom = "Sequence"

  protected def buildChildUnparsers: IndexedSeq[SequenceChildUnparser]

  /**
   * Builds an entire sequence's children, scalar and array/optional alike,
   * without separators, suppression, or any other write-only content, since
   * none of that produces infoset nodes.
   */
  final override def build(state: UState): Unit = {
    state.groupIndexStack.push(1L)

    val cus = buildChildUnparsers
    var index = 0
    val limit = cus.length
    while (index < limit) {
      val cu = cus(index)
      val trd = cu.trd
      state.pushTRD(trd)
      cu match {
        case rep: RepeatingChildUnparser => {
          state.arrayIterationIndexStack.push(1L)
          state.occursIndexStack.push(1L)
          val erd = rep.erd
          var numOccurrences = 0
          val maxReps = rep.maxRepeats(state)

          Assert.invariant(state.inspect, "No event for building.")
          val ev = state.inspectAccessor
          if (ev.erd eq erd) {
            rep.startArrayOrOptional(state)
            while (rep.shouldDoUnparser(rep, state)) {
              rep.childUnparser.build(state)
              numOccurrences += 1
              state.moveOverOneArrayIterationIndexOnly()
              state.moveOverOneOccursIndexOnly()
              state.moveOverOneGroupIndexOnly()
            }
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              state.arrayIterationPos - 1
            )
            rep.endArrayOrOptional(erd, state)
          } else {
            rep.checkFinalOccursCountBetweenMinAndMaxOccurs(
              state,
              rep,
              numOccurrences,
              maxReps,
              0
            )
          }

          state.arrayIterationIndexStack.pop()
          state.occursIndexStack.pop()
        }
        case scalarUnparser => {
          scalarUnparser.childUnparser.build(state)
          trd match {
            case erd: ElementRuntimeData if !erd.isRepresented => // ok, skip group advance
            case _ => state.moveOverOneGroupIndexOnly()
          }
        }
      }
      state.popTRD(trd)
      index += 1
    }

    state.groupIndexStack.pop()
  }
}
