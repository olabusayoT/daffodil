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

import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker

/**
 * Shared BuildState construction for tests. The default suspension-wait
 * thresholds are doubled, since one tracker serves both build and write.
 */
object UnparseSharedContextTestFixture {
  def build(dp: DataProcessor, prefetchLimit: Long)(
    suspensionWaitYoung: Int = dp.tunables.unparseSuspensionWaitYoung * 2,
    suspensionWaitOld: Int = dp.tunables.unparseSuspensionWaitOld * 2
  ): UnparseSharedContext = {
    new UnparseSharedContext(
      new SuspensionTracker(suspensionWaitYoung, suspensionWaitOld),
      dp,
      dp.tunables,
      prefetchLimit
    )
  }
}
