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

package org.apache.daffodil.section02.schema_definition_errors

import org.apache.daffodil.junit.tdml.TdmlSuite
import org.apache.daffodil.junit.tdml.TdmlTests

import org.junit.Test

object TestSDE extends TdmlSuite {
  val tdmlResource =
    "/org/apache/daffodil/section02/schema_definition_errors/SchemaDefinitionErrors.tdml"
}

class TestSDE extends TdmlTests {
  val tdmlSuite = TestSDE

  @Test def AS000_rev() = test()

  @Test def schema_component_err() = test()

  @Test def schema_line_number() = test()
  @Test def schema_warning() = test()
  @Test def missing_appinfo_source() = test()
  @Test def missing_appinfo_source_nondfdl() = test()
  @Test def missing_closing_tag() = test()
  @Test def ignoreAttributeFormDefault() = test()

  @Test def schema_warning_locally_suppressed() = test()

  @Test def schema_warning_escalated_to_error() = test()

  @Test def schema_warning_escalated_to_error2() = test()
}
