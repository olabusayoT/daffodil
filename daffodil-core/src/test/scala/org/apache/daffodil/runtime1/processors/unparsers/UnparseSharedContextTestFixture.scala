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

import java.io.ByteArrayOutputStream
import scala.jdk.CollectionConverters.*
import scala.xml.Node

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.runtime1.infoset.DIArray
import org.apache.daffodil.runtime1.infoset.DIComplex
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.DINode
import org.apache.daffodil.runtime1.infoset.InfosetInputter
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.DataProcessor
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.unparsers.runtime1.ElementUnparserBase

/**
 * Shared InfosetBuildState construction for tests, with the suspension-wait
 * thresholds defaulting to the tunables' values.
 */
object UnparseSharedContextTestFixture {
  def build(dp: DataProcessor, prefetchLimit: Long)(
    suspensionWaitYoung: Int = dp.tunables.unparseSuspensionWaitYoung,
    suspensionWaitOld: Int = dp.tunables.unparseSuspensionWaitOld
  ): UnparseSharedContext = {
    new UnparseSharedContext(
      new SuspensionTracker(suspensionWaitYoung, suspensionWaitOld),
      dp.tunables,
      prefetchLimit
    )
  }

  private def throwDiagnostics(ds: java.util.List[api.Diagnostic]): Nothing =
    throw new Exception(ds.asScala.map(_.getMessage()).mkString("\n"))

  /**
   * Compiles testSchema with the given tunables and returns the resulting
   * DataProcessor without a saveAndReload round-trip, since the tests build
   * state directly off the live object.
   */
  def compileForUnparse(
    testSchema: Node,
    tunables: Map[String, String] = Map.empty
  ): DataProcessor = {
    val pf = Compiler().withTunables(tunables).compileNode(testSchema)
    if (pf.isError) throwDiagnostics(pf.getDiagnostics)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) throwDiagnostics(dp.getDiagnostics)
    dp
  }

  /**
   * Builds a fresh InfosetInputter walking infosetXML against dp, already
   * initialized with the root TRD pushed.
   */
  def newInitializedInputter(infosetXML: Node, dp: DataProcessor): InfosetInputter = {
    val inputter = new InfosetInputter(new ScalaXMLInfosetInputter(infosetXML))
    inputter.initialize(dp.ssrd.elementRuntimeData, dp.tunables)
    inputter
  }

  /**
   * Unparses infosetXML in a single pass (dp must have releaseUnneededInfoset
   * disabled, so the built tree survives), then re-walks that tree through
   * a fresh UState that builds no infoset, via unparseTree. Returns
   * (singlePassBytes, unparseTreeBytes) for the caller to assert equality on.
   */
  def getSinglePassAndUnparseTreeBytes(
    dp: DataProcessor,
    infosetXML: Node,
    prefetchLimit: Long = 1000
  ): (Array[Byte], Array[Byte]) = {
    val singlePassOut = new ByteArrayOutputStream()
    val singlePassResult = dp.unparse(new ScalaXMLInfosetInputter(infosetXML), singlePassOut)
    if (singlePassResult.isError) throwDiagnostics(singlePassResult.getDiagnostics)
    val singlePassBytes = singlePassOut.toByteArray

    val ustate = singlePassResult.resultState.asInstanceOf[UStateMain]
    val builtTree: DIDocument = ustate.documentElement

    val unparseTreeOut = new ByteArrayOutputStream()
    val unparseTreeInputter = newInitializedInputter(infosetXML, dp)
    val unparseTreeState =
      UState.createInitialUState(unparseTreeOut, dp, unparseTreeInputter, false)
    unparseTreeState.getDataOutputStream.setPriorBitOrder(
      dp.ssrd.elementRuntimeData.defaultBitOrder
    )

    val sharedCtx = new UnparseSharedContext(
      new SuspensionTracker(
        dp.tunables.unparseSuspensionWaitYoung,
        dp.tunables.unparseSuspensionWaitOld
      ),
      dp.tunables,
      prefetchLimit
    )
    unparseTreeState.setSharedContext(sharedCtx)

    primeLeadCounter(sharedCtx, builtTree.child(0))

    val rootUnparser = dp.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    val rootNode = sharedCtx.awaitChild(builtTree, 0)
    rootUnparser.unparseTree(rootNode, unparseTreeState)
    unparseTreeState.evalSuspensions(isFinal = true)
    unparseTreeState.getDataOutputStream.setFinished(unparseTreeState)

    (singlePassBytes, unparseTreeOut.toByteArray)
  }

  /**
   * Pre-increments UnparseSharedContext's lead counter once per element in
   * node's subtree, for a tree built outside InfosetBuildState (unparseTree's
   * decrementLead call requires the counter already be symmetric).
   */
  private def primeLeadCounter(sharedCtx: UnparseSharedContext, node: DINode): Unit =
    node match {
      case complex: DIComplex =>
        sharedCtx.incrementLead()
        var i = 0
        while (i < complex.numChildren) {
          primeLeadCounter(sharedCtx, complex.child(i))
          i += 1
        }
      case array: DIArray =>
        var i = 0
        while (i < array.numChildren) {
          primeLeadCounter(sharedCtx, array.child(i))
          i += 1
        }
      case _ =>
        sharedCtx.incrementLead()
    }
}
