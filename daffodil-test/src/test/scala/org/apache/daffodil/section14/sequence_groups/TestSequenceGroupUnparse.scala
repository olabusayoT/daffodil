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

package org.apache.daffodil.section14.sequence_groups

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestSequenceGroupUnparse extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section14/sequence_groups/SequenceGroupUnparse.tdml"
}

class TestSequenceGroupUnparse extends TdmlTests {
  val tdmlSuite = TestSequenceGroupUnparse

  @Test def seqWithOptionals1() = test()
  @Test def seqWithOptionals2() = test()
  @Test def seqWithOptionals3() = test()
  @Test def seqWithOptionals4() = test()
  @Test def seqWithOptionals5() = test()

  @Test def seqWithHiddenGroupContainingComplex() = test()
}
