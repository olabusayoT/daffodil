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

package org.apache.daffodil.section12.lengthKind

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestLengthKindImplicit extends TdmlSuite {
  val tdmlResource = "/org/apache/daffodil/section12/lengthKind/implicit.tdml"
}

class TestLengthKindImplicit extends TdmlTests {
  val tdmlSuite = TestLengthKindImplicit

  @Test def nested_seq() = test()
  @Test def nested_seq_01() = test()

  @Test def implicit_with_len() = test()
  @Test def implicit_ignored_len() = test()
  @Test def implicitLenTime() = test()
}
