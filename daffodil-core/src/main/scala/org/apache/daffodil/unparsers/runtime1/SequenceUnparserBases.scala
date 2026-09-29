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

import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.processors.SequenceRuntimeData
import org.apache.daffodil.runtime1.processors.unparsers.*

abstract class OrderedSequenceUnparserBase(
  srd: SequenceRuntimeData
) extends CombinatorUnparser(srd) {
  override def nom = "Sequence"

  // False when no more children will ever come, or when a different term's
  // child appeared in this tree position instead.
  protected final def hasOccurrences(
    rep: RepeatingChildUnparser,
    complex: DIComplex,
    idx: Int,
    sharedCtx: UnparseSharedContext
  ): Boolean =
    sharedCtx.childExistsOrFinal(complex, idx) && (complex.child(idx).erd eq rep.erd)

  protected final def isGroupTerm(cu: SequenceChildUnparser): Boolean =
    !cu.childUnparser.isInstanceOf[ElementUnparserBase] &&
      cu.childUnparser.isInstanceOf[WriteUnparser]

  // A nested bare group (e.g. a choice) may resolve to a branch with no
  // infoset footprint at all; once build is done and no child showed up, the
  // group's own dispatch decides. A plain element term always needs an
  // actual child, so it always waits.
  protected final def awaitRequiredTermChild(
    isGroupTerm: Boolean,
    complex: DIComplex,
    idx: Int,
    sharedCtx: UnparseSharedContext
  ): Unit = {
    if (!isGroupTerm || sharedCtx.childExistsOrFinal(complex, idx)) {
      sharedCtx.awaitChild(complex, idx)
    }
  }
}
