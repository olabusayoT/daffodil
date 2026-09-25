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

/**
 * Builds the infoset structure a grammar production is responsible for (if
 * any), consuming events from the InfosetInputter as needed, without
 * evaluating expressions, writing bytes, or creating suspensions.
 *
 * Unlike Unparser, most grammar productions have no Builder at all: a
 * Gram's `builder` val (see GramRuntime1Mixin) defaults to Maybe.Nope, and
 * only productions that create infoset nodes or decide which child is
 * structurally present (elements, sequences, choices) construct a real one.
 * The Builder tree is therefore much sparser than the Unparser tree.
 */
trait Builder extends Serializable {
  def build(state: UState): Unit
}

/**
 * Runs a fixed list of child builders in order. The Gram-level analog of
 * SeqCompUnparser, constructed by SeqComp when 2+ of its children have a
 * builder.
 */
final class SeqCompBuilder(childBuilders: Array[Builder]) extends Builder {
  override def build(state: UState): Unit = {
    var i = 0
    while (i < childBuilders.length) {
      childBuilders(i).build(state)
      i += 1
    }
  }
}
