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

import java.io.ByteArrayOutputStream

import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.io.DirectOrBufferedDataOutputStream
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.DIDocument
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter
import org.apache.daffodil.runtime1.processors.SuspensionTracker
import org.apache.daffodil.runtime1.processors.VariableBox
import org.apache.daffodil.runtime1.processors.unparsers.BuildFinished
import org.apache.daffodil.runtime1.processors.unparsers.UStateMain
import org.apache.daffodil.runtime1.processors.unparsers.UnparseSharedContext
import org.apache.daffodil.runtime1.processors.unparsers.UnparseSharedContextTestFixture

import org.junit.Assert.*
import org.junit.Test

/**
 * Validates write's side of build/write-prefetch: writing an
 * already-built tree matches a real single-pass unparse, byte for byte.
 */
class WriteContentWalkerTest {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testWriteWalkerMatchesActualUnparse(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes" outputNewLine="%CR;%LF;"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="name" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="age" type="xs:string" dfdl:lengthKind="delimited"/>
            <xs:element name="city" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )

    val infoset =
      <ex:row xmlns:ex={example}><name>Alice</name><age>30</age><city>Boston</city></ex:row>

    val dp = TestUtils.compileForUnparse(sch)

    // Ground truth: the actual, unmodified single-pass unparse, with default
    // (production) tunables, including releaseUnneededInfoset, which frees
    // nodes as unparse goes.
    val groundTruthOut = new ByteArrayOutputStream()
    val groundTruthResult = dp.unparse(new ScalaXMLInfosetInputter(infoset), groundTruthOut)
    assertFalse(groundTruthResult.getDiagnostics.toString, groundTruthResult.isError)
    val groundTruthBytes = groundTruthOut.toByteArray

    // Second unparse, releaseUnneededInfoset disabled, purely to obtain an
    // intact DIDocument tree to walk independently (the default frees the
    // tree by completion). The actual build/write-prefetch path never frees
    // during build; only write does, after emitting a node's content.
    val dpNoFree =
      TestUtils.compileForUnparse(sch, Map("releaseUnneededInfoset" -> "false"))
    val discardOut = new ByteArrayOutputStream()
    val treeResult = dpNoFree.unparse(new ScalaXMLInfosetInputter(infoset), discardOut)
    assertFalse(treeResult.getDiagnostics.toString, treeResult.isError)
    val ustate = treeResult.resultState.asInstanceOf[UStateMain]
    val builtTree: DIDocument = ustate.documentElement

    // Redirect ustate's DataOutputStream to a fresh sink, then write the
    // ALREADY-BUILT tree directly instead of an actual unparse pass.
    val walkerOut = new ByteArrayOutputStream()
    val freshDos = DirectOrBufferedDataOutputStream(
      walkerOut,
      null,
      false,
      ustate.tunable.outputStreamChunkSizeInBytes,
      ustate.tunable.maxByteArrayOutputStreamBufferSizeInBytes,
      ustate.tunable.tempFilePath
    )
    // Mirrors DataProcessor.unparse's setup. A freshly constructed DOS has
    // no prior bit order until told one.
    freshDos.setPriorBitOrder(dpNoFree.ssrd.elementRuntimeData.defaultBitOrder)
    ustate.setDataOutputStream(freshDos)

    // writeContent requires an actual UnparseSharedContext (awaitChild,
    // decrementLead, etc. are methods on it); the tree is already fully
    // built, so a minimal one suffices; observeBuildSignal marks it as
    // such, since this test never runs an actual build coroutine.
    val sharedCtx =
      new UnparseSharedContext(
        builtTree,
        new VariableBox(dpNoFree.variableMap.copy()),
        new SuspensionTracker(
          dpNoFree.tunables.unparseSuspensionWaitYoung,
          dpNoFree.tunables.unparseSuspensionWaitOld
        ),
        dpNoFree,
        dpNoFree.tunables,
        prefetchLimit = 1000
      )
    sharedCtx.observeBuildSignal(BuildFinished)
    ustate.setSharedContext(sharedCtx)

    UnparseSharedContextTestFixture.primeLeadCounter(sharedCtx, builtTree.child(0))

    val rootUnparser = dpNoFree.ssrd.unparser.asInstanceOf[ElementUnparserBase]
    val rootNode = sharedCtx.awaitChild(builtTree, 0)
    rootUnparser.writeContent(rootNode, ustate)
    // Mirrors unparseViaBuildThenWrite's ordering: this schema's default
    // separatorSuppressionPolicy ("anyEmpty") writes separators via actual
    // suspensions that must be drained before the DOS is finalized, or the
    // speculative regions never resolve.
    ustate.evalSuspensions(isFinal = true)
    // Mirrors DataProcessor's writeState.getDataOutputStream.setFinished.
    // Separator suspensions split the stream into a chain of DOS objects,
    // so by the time writing is done, ustate's current DataOutputStream is
    // no longer necessarily freshDos.
    ustate.getDataOutputStream.setFinished(ustate)

    val walkerBytes = walkerOut.toByteArray

    assertArrayEquals(groundTruthBytes, walkerBytes)
  }
}
