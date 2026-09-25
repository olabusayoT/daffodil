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
import org.apache.daffodil.lib.util.Maybe
import org.apache.daffodil.runtime1.processors.ElementRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

/**
 * Builds an entire sequence's children, scalar and array/optional alike,
 * without separators, suppression, or any other write-only content, since
 * none of that produces infoset nodes. Reuses RepeatingChildUnparser's
 * existing shouldDoUnparser/startArrayOrOptional/endArrayOrOptional/
 * checkFinalOccursCountBetweenMinAndMaxOccurs unchanged: those only touch
 * state's inspect/advanceOrError and index-stack bookkeeping, all of which
 * build needs exactly as-is to navigate the same event stream.
 *
 * children pairs each sequence child's unparser (needed for the above
 * dispatch/bookkeeping) with that child's own Builder, if it has one.
 */
final class SequenceBuilder(children: Array[(SequenceChildUnparser, Maybe[Builder])])
  extends Builder {

  override def build(state: UState): Unit = {
    state.groupIndexStack.push(1L)

    var index = 0
    val limit = children.length
    while (index < limit) {
      val (cu, childBuilder) = children(index)
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
              if (childBuilder.isDefined) childBuilder.get.build(state)
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
        case _ => {
          if (childBuilder.isDefined) childBuilder.get.build(state)
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
