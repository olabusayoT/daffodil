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

package org.apache.daffodil.runtime1.processors

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

import org.apache.daffodil.api
import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.core.util.TestUtils
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.externalvars.ExternalVariablesLoader
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter

import org.junit.Assert.*
import org.junit.Test

/**
 * Exercises DataProcessor.unparse with useBuildWritePrefetch enabled,
 * confirming byte-identical output vs. single-pass across many schema shapes.
 */
class TestBuildWritePrefetchDataProcessor {

  val example = XMLUtils.EXAMPLE_NAMESPACE

  @Test def testOVCSuspensionSchemaMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><actual>005</actual></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    // dfdl:textNumberPadCharacter="0"/textNumberJustification="right" produces
    // actual, schema-configured zero-padding, not generic fillByte-based padding.
    assertEquals("006005", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  @Test def testArrayChoiceSeparatorSchemaMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
              <xs:element name="header" type="xs:string" dfdl:lengthKind="delimited"/>
              <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
                dfdl:lengthKind="delimited"
                dfdl:occursCountKind="implicit"/>
              <xs:choice>
                <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
                <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
              </xs:choice>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <header>H</header>
        <item>a</item>
        <item>b</item>
        <item>c</item>
        <typeB>X</typeB>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("H,a,b,c,X", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A missing optional occurrence has no content, so its positionally
  // required separator must be suppressed under trailingEmptyStrict even
  // when the optional element's own separator is statically unsuppressible.
  @Test def testAbsentTrailingOptionalSuppressesSeparator(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        lengthKind="delimited"/>,
      <xs:element name="row" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix"
            dfdl:separatorSuppressionPolicy="trailingEmptyStrict">
            <xs:element name="item" type="xs:string" maxOccurs="unbounded"
              dfdl:lengthKind="delimited"
              dfdl:occursCountKind="parsed"/>
            <xs:element name="endNL" type="xs:string" minOccurs="0"
              dfdl:lengthKind="explicit" dfdl:length="0"
              dfdl:occursCountKind="implicit"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <item>a</item>
        <item>b</item>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("a,b", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A choice reached while withinHiddenNest: both sides pick
  // defaultUnparser unconditionally, since the outcome is schema-determined.
  @Test def testHiddenChoiceMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        lengthKind="delimited"/>,
      <xs:element name="row" dfdl:lengthKind="delimited">
        <xs:complexType>
          <xs:sequence dfdl:separator=",">
            <xs:sequence dfdl:hiddenGroupRef="tns:hiddenChoiceGroup"/>
            <xs:element name="g" type="xs:int"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="hiddenChoiceGroup">
        <xs:choice>
          <xs:element name="e" type="xs:string"
            dfdl:initiator="["
            dfdl:outputValueCalc="{ 'hello' }"/>
          <xs:element name="f" type="xs:int" dfdl:outputValueCalc="{ 1 }"/>
        </xs:choice>
      </xs:group>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><g>3</g></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("[hello,3", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A postfix-separated array as the last term: the separator must
  // still be written after the final occurrence, not dropped.
  @Test def testTrailingArrayPostfixSeparatorMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="%NL;" dfdl:separatorPosition="postfix">
              <xs:element name="item" type="xs:string" maxOccurs="unbounded"
                dfdl:lengthKind="delimited"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <item>a</item>
        <item>b</item>
        <item>c</item>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("a\nb\nc\n", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A choice branch with zero tree footprint (absent optional element)
  // must still write its own initiator, or write's dispatch can hang.
  @Test def testChoiceBranchWithAbsentOptionalElementMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      Seq(
        <xs:group name="optElement">
          <xs:sequence>
            <xs:element name="opt" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:group>,
        <xs:element name="e1" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence>
                <xs:choice>
                  <xs:group dfdl:initiator="first_defaultable" ref="tns:optElement"/>
                  <xs:element name="req" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
                </xs:choice>
                <xs:element name="after" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
              </xs:sequence>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:e1 xmlns:ex={example}><after>1</after></ex:e1>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("first_defaultable1", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  /** Same schema as above, but with "opt" PRESENT (an actual match, not the empty-branch fallback). */
  @Test def testChoiceBranchWithPresentOptionalElementMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      Seq(
        <xs:group name="optElement">
          <xs:sequence>
            <xs:element name="opt" type="xs:string" minOccurs="0" dfdl:lengthKind="explicit" dfdl:length="1"/>
          </xs:sequence>
        </xs:group>,
        <xs:element name="e1" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence>
                <xs:choice>
                  <xs:group dfdl:initiator="first_defaultable" ref="tns:optElement"/>
                  <xs:element name="req" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
                </xs:choice>
                <xs:element name="after" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
              </xs:sequence>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:e1 xmlns:ex={example}>
        <opt>0</opt>
        <after>1</after>
      </ex:e1>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("first_defaultable01", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  @Test def testManyOccurrenceArrayWithSmallPrefetchLimitMatchesSinglePass(): Unit = {
    val numItems = 40
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
              <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="unbounded"
                dfdl:lengthKind="delimited"
                dfdl:occursCountKind="implicit"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val items = (0 until numItems).map(i => <item>{s"i$i"}</item>)
    val infoset =
      <ex:row xmlns:ex={example}>
        {items}
      </ex:row>

    // A tiny lookahead window makes build stop and restart many times across
    // this array's 40 occurrences; the output must still match single-pass
    // byte for byte.
    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(
      sch,
      infoset,
      extraTunables = Map("unparsePrefetchWindowNodes" -> "3")
    )
    assertEquals(
      (0 until numItems).map(i => s"i$i").mkString(","),
      new String(singlePassBytes, StandardCharsets.US_ASCII)
    )
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A bare nested xs:sequence has no tree node of its own; the enclosing
  // writeContent must not advance/free a tree-child position recursing into it.
  @Test def testNestedBareSequenceMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
              <xs:element name="before" type="xs:string" dfdl:lengthKind="delimited"/>
              <xs:sequence>
                <xs:element name="inner1" type="xs:string" dfdl:lengthKind="delimited"/>
                <xs:element name="inner2" type="xs:string" dfdl:lengthKind="delimited"/>
              </xs:sequence>
              <xs:element name="after" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <before>B</before>
        <inner1>I1</inner1>
        <inner2>I2</inner2>
        <after>A</after>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    // The nested xs:sequence has no dfdl:separator of its own and doesn't inherit the
    // outer's "," (a local property of the outer xs:sequence's annotation, not pushed
    // down to nested groups), so it compiles as unseparated: no separator between
    // inner1/inner2, but the outer's separator still appears before/after the nested group.
    assertEquals("B,I1I2,A", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A fixed-length choice must pad unused space after a shorter branch,
  // not just write the branch content.
  @Test def testFixedLengthChoicePaddingMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:choice dfdl:choiceLengthKind="explicit" dfdl:choiceLength="5">
              <xs:element name="typeA" type="xs:string" dfdl:lengthKind="delimited"/>
              <xs:element name="typeB" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:choice>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    // typeB is 2 bytes; the choice's declared length is 5 bytes, so 3
    // bytes of filler should follow it.
    val infoset = <ex:row xmlns:ex={example}><typeB>XY</typeB></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals(5, singlePassBytes.length)
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A hidden group's elements never get hasValue=true; write's dispatch
  // must special-case that instead of blocking on it forever.
  @Test def testHiddenGroupMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      Seq(
        <xs:group name="g1">
          <xs:sequence>
            <xs:element name="hidden1" type="xs:string"
              dfdl:lengthKind="explicit"
              dfdl:length="1"
              dfdl:outputValueCalc="{ 'H' }"/>
          </xs:sequence>
        </xs:group>,
        <xs:element name="row" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:g1"/>
              <xs:element name="visible" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><visible>V</visible></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("HV", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Initiator/terminator with no separator: same delimiter-frame code
  // path as the separator tests above, via a different property combination.
  @Test def testInitiatorTerminatorMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:initiator="[" dfdl:terminator="]">
              <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
              <xs:element name="b" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <a>A</a>
        <b>B</b>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("[AB]", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A nillable complex element whose model group is empty has a non-empty
  // content gram only because of its initiator and terminator, so its
  // content builder builds nothing.
  private val emptyNillableComplexSchema = SchemaUtils.dfdlTestSchema(
    <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
    <dfdl:format ref="tns:GeneralFormat"
      encoding="ascii"
      lengthUnits="bytes"/>,
    <xs:element name="row" nillable="true" dfdl:lengthKind="implicit"
      dfdl:nilKind="literalValue"
      dfdl:nilValue="%ES;"
      dfdl:nilValueDelimiterPolicy="none"
      dfdl:initiator="["
      dfdl:terminator="]">
      <xs:complexType>
        <xs:sequence/>
      </xs:complexType>
    </xs:element>,
    elementFormDefault = "unqualified"
  )

  @Test def testNillableComplexWithEmptyContentNotNilledMatchesSinglePass(): Unit = {
    val infoset = <ex:row xmlns:ex={example}/>
    val (singlePassBytes, prefetchBytes) =
      TestUtils.getSinglePassAndPrefetchBytes(emptyNillableComplexSchema, infoset)
    assertEquals("[]", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  @Test def testNillableComplexWithEmptyContentNilledMatchesSinglePass(): Unit = {
    val infoset =
      <ex:row xmlns:ex={
        example
      } xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:nil="true"/>
    val (singlePassBytes, prefetchBytes) =
      TestUtils.getSinglePassAndPrefetchBytes(emptyNillableComplexSchema, infoset)
    assertEquals("", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A hidden group with no members has nothing to build, but still has to
  // balance the hidden-depth counter around its body.
  @Test def testHiddenGroupWithEmptyBodyMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      Seq(
        <xs:group name="g1">
          <xs:sequence/>
        </xs:group>,
        <xs:element name="row" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence dfdl:hiddenGroupRef="ex:g1"/>
              <xs:element name="visible" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><visible>V</visible></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("V", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Escape-scheme state is confined entirely to unparsers that build no infoset events;
  // confirms that end to end with no frame changes needed.
  @Test def testEscapeSchemeMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>
      <dfdl:defineEscapeScheme name="pound">
        <dfdl:escapeScheme
          escapeCharacter='#'
          escapeKind="escapeCharacter"
          escapeEscapeCharacter=""
          extraEscapedCharacters=""
          generateEscapeBlock="whenNeeded"/>
      </dfdl:defineEscapeScheme>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator=",">
              <xs:element name="s1" type="xs:string"
                dfdl:lengthKind="delimited"
                dfdl:escapeSchemeRef="pound"/>
              <xs:element name="s2" type="xs:string" dfdl:lengthKind="delimited"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <s1>one, two</s1>
        <s2>three</s2>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("one#, two,three", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A dfdlx:layer-wrapped sequence: confirms the layer's DOS-splitting
  // setup produces byte-identical output under prefetch.
  @Test def testLayeredSequenceMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
      <xs:import
        namespace="urn:org.apache.daffodil.layers.fixedLength"
        schemaLocation="/org/apache/daffodil/layers/xsd/fixedLengthLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        lengthKind="delimited"/>,
      <xs:element name="e1"
        dfdl:lengthKind="implicit"
        xmlns:fl="urn:org.apache.daffodil.layers.fixedLength">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:sequence dfdlx:layer="fl:fixedLength">
                <xs:annotation>
                  <xs:appinfo source="http://www.ogf.org/dfdl/">
                    <dfdl:newVariableInstance ref="fl:fixedLength" defaultValue="8"/>
                  </xs:appinfo>
                </xs:annotation>
                <xs:element name="s1" type="xs:string"/>
              </xs:sequence>
              <xs:element name="after" type="xs:string"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    // s1 is exactly 8 bytes (the layer's declared fixedLength), long enough that
    // FixedLengthOutputStream.write's accumulate-then-auto-close-on-count-==fixedLength
    // logic runs across several bytes, not just one or two; a short value could pass
    // while still masking an off-by-one in the accumulation.
    val infoset =
      <ex:e1 xmlns:ex={example}>
        <s1>ABCDEFGH</s1>
        <after>Q</after>
      </ex:e1>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("ABCDEFGHQ", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A layer's length-exceeded error must surface under prefetch as it does
  // in single-pass, not hang or leak a raw exception.
  @Test def testLayeredSequenceLengthMismatchErrorMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
      <xs:import
        namespace="urn:org.apache.daffodil.layers.fixedLength"
        schemaLocation="/org/apache/daffodil/layers/xsd/fixedLengthLayer.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        lengthKind="delimited"/>,
      <xs:element name="e1"
        dfdl:lengthKind="implicit"
        xmlns:fl="urn:org.apache.daffodil.layers.fixedLength">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:sequence dfdlx:layer="fl:fixedLength">
                <xs:annotation>
                  <xs:appinfo source="http://www.ogf.org/dfdl/">
                    <dfdl:newVariableInstance ref="fl:fixedLength" defaultValue="8"/>
                  </xs:appinfo>
                </xs:annotation>
                <xs:element name="s1" type="xs:string"/>
              </xs:sequence>
              <xs:element name="after" type="xs:string"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    // s1 is 10 bytes, 2 more than the layer's declared fixedLength=8.
    val infoset =
      <ex:e1 xmlns:ex={example}>
        <s1>ABCDEFGHIJ</s1>
        <after>Q</after>
      </ex:e1>

    val singlePassDp = Compiler()
      .withTunable("useBuildWritePrefetch", "false")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    val singlePassOut = new ByteArrayOutputStream()
    val singlePassRes =
      singlePassDp.unparse(new ScalaXMLInfosetInputter(infoset), singlePassOut)
    assertTrue("expected a failed UnparseResult, not a successful one", singlePassRes.isError)
    assertTrue(
      singlePassRes.getDiagnostics
        .get(0)
        .getMessage
        .contains("exceeded fixed layer length of 8")
    )

    val prefetchDp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    val prefetchOut = new ByteArrayOutputStream()
    val prefetchRes = prefetchDp.unparse(new ScalaXMLInfosetInputter(infoset), prefetchOut)
    assertTrue("expected a failed UnparseResult, not a successful one", prefetchRes.isError)
    assertTrue(
      prefetchRes.getDiagnostics
        .get(0)
        .getMessage
        .contains("exceeded fixed layer length of 8")
    )
  }

  // A chained suspension: computed1's OVC references computed2 (itself
  // an OVC forward reference), two levels deep instead of one.
  @Test def testNestedOVCSuspensionsMatchSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed1" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../computed2 + 1 }"/>
            <xs:element name="computed2" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><actual>005</actual></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("007006005", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Binary integers (every other test here is textual), through an
  // array to also exercise the repeating-child frame path.
  @Test def testBinaryIntArrayMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        byteOrder="bigEndian"
        lengthUnits="bytes"
        representation="binary"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:element name="item" type="xs:int" minOccurs="0" maxOccurs="unbounded"
                dfdl:lengthKind="explicit"
                dfdl:length="4"
                dfdl:occursCountKind="implicit"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <item>1</item>
        <item>2</item>
        <item>3</item>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertArrayEquals(
      Array[Byte](0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3),
      singlePassBytes
    )
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A variable-length dfdl:length expression (every other test here is
  // constant), exercising computeTargetLength's non-constant branch.
  @Test def testVariableLengthExpressionMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:element name="lenField" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
              <xs:element name="content" type="xs:string"
                dfdl:lengthKind="explicit"
                dfdl:length="{ xs:int(../lenField) }"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <lenField>5</lenField>
        <content>hello</content>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("05hello", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A prefixed-length element: the prefix depends on the content's
  // unparsed length, a forward dependency like OVC but length-driven.
  @Test def testPrefixedLengthMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      Seq(
        <xs:simpleType name="lenPrefixType"
          dfdl:representation="text"
          dfdl:lengthKind="explicit"
          dfdl:length="2"
          dfdl:lengthUnits="bytes">
          <xs:restriction base="xs:int"/>
        </xs:simpleType>,
        <xs:element name="row" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence>
                <xs:element name="content" type="xs:string"
                  dfdl:lengthKind="prefixed"
                  dfdl:prefixLengthType="tns:lenPrefixType"
                  dfdl:lengthUnits="bytes"
                  dfdl:prefixIncludesPrefixLength="no"/>
              </xs:sequence>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><content>hello</content></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("05hello", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A prefixed-length element with complex (not simple) content, so
  // write's dispatch must recurse into the wrapped content unparser.
  @Test def testPrefixedLengthComplexContentMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      Seq(
        <xs:simpleType name="lenPrefixType2"
          dfdl:representation="text"
          dfdl:lengthKind="explicit"
          dfdl:length="2"
          dfdl:lengthUnits="bytes">
          <xs:restriction base="xs:int"/>
        </xs:simpleType>,
        <xs:element name="row2" dfdl:lengthKind="implicit">
          <xs:complexType>
            <xs:sequence>
              <xs:sequence>
                <xs:element name="body"
                  dfdl:lengthKind="prefixed"
                  dfdl:prefixLengthType="tns:lenPrefixType2"
                  dfdl:lengthUnits="bytes"
                  dfdl:prefixIncludesPrefixLength="no">
                  <xs:complexType>
                    <xs:sequence dfdl:separator=",">
                      <xs:element name="a" type="xs:string" dfdl:lengthKind="delimited"/>
                      <xs:element name="b" type="xs:string" dfdl:lengthKind="delimited"/>
                    </xs:sequence>
                  </xs:complexType>
                </xs:element>
              </xs:sequence>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      ),
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row2 xmlns:ex={example}>
        <body>
          <a>hi</a>
          <b>bye</b>
        </body>
      </ex:row2>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("06hi,bye", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // An OVC referencing fn:count of a preceding array, resolvable
  // directly against the tree: the ordinary non-suspending OVC path.
  @Test def testOVCCountOfPrecedingArrayMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="items" type="xs:int" minOccurs="0" maxOccurs="3"
              dfdl:lengthKind="explicit"
              dfdl:length="1"
              dfdl:occursCountKind="implicit"/>
            <xs:element name="cnt" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="1"
              dfdl:outputValueCalc="{ fn:count(../items) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <items>1</items>
        <items>2</items>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("122", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A large array that build finishes entirely before write starts, so
  // write never has to wait for more tree.
  @Test def testOVCCountOfPrecedingArrayAfterBuildFullyFinishesMatchesSinglePass(): Unit = {
    val numItems = 500
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="items" type="xs:int" minOccurs="0" maxOccurs="unbounded"
              dfdl:lengthKind="explicit"
              dfdl:length="1"
              dfdl:occursCountKind="implicit"/>
            <xs:element name="cnt" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ fn:count(../items) }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val items = (0 until numItems).map(i => <items>{i % 10}</items>)
    val infoset =
      <ex:row xmlns:ex={example}>
        {items}
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A dynamic dfdl:terminator expression referencing a preceding
  // array's count: navigation-only, resolvable directly against the tree.
  @Test def testDynamicTerminatorReferencingArrayCountMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence>
              <xs:element name="int" type="xs:int" minOccurs="0" maxOccurs="2"
                dfdl:lengthKind="explicit"
                dfdl:length="1"/>
              <xs:element name="term" type="xs:string"
                dfdl:lengthKind="explicit"
                dfdl:length="0"
                dfdl:terminator="{ if (fn:count(../int) gt 1) then 'x' else 'y' }"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><term></term></ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("y", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // An IVC field (no unparse effect) referencing fn:exists over a
  // nested array: exercises ordinary navigation past a nested array.
  @Test def testIVCExistsOverNestedArrayMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="|">
              <xs:element name="seq">
                <xs:complexType>
                  <xs:sequence dfdl:separator=",">
                    <xs:element name="item" type="xs:string" minOccurs="0" maxOccurs="10"
                      dfdl:lengthKind="delimited"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
              <xs:element name="exists" type="xs:boolean"
                dfdl:inputValueCalc="{ fn:exists(../seq/item) }"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <seq>
          <item>1</item>
          <item>2</item>
          <item>3</item>
          <item>4</item>
        </seq>
        <exists>true</exists>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("1,2,3,4", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Aggressive throttling forces every "len" suspension to be retried
  // repeatedly; guards against re-running an already-resolved one.
  @Test def testManyValueLengthForwardReferencesWithAggressiveThrottlingMatchesSinglePass()
    : Unit = {
    val numRecords = 25
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="record" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence dfdl:separator="|">
                  <xs:element name="len" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
                  <xs:element name="data" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val records = (0 until numRecords).map(i => <record><data>{s"value$i"}</data></record>)
    val infoset =
      <ex:root xmlns:ex={example}>
        {records}
      </ex:root>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(
      sch,
      infoset,
      extraTunables = Map(
        "unparsePrefetchWindowNodes" -> "2",
        "unparseSuspensionWaitYoung" -> "1",
        "unparseSuspensionWaitOld" -> "1"
      )
    )
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A repeating NVI scope with a valueLength OVC reading it, letting
  // build race many iterations ahead: guards against a stale iteration's value.
  @Test def testNVIScopedVariableWithValueLengthOVCMatchesSinglePass(): Unit = {
    val numRecords = 25
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>, {
        <dfdl:format ref="tns:GeneralFormat"
          encoding="ascii"
          lengthUnits="bytes"/>
        <dfdl:defineVariable name="runningVar" type="xs:int"/>
      },
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="record" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence dfdl:separator="|">
                  <xs:annotation>
                    <xs:appinfo source="http://www.ogf.org/dfdl/">
                      <dfdl:newVariableInstance ref="tns:runningVar" defaultValue="0"/>
                    </xs:appinfo>
                  </xs:annotation>
                  <xs:element name="idx" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2">
                    <xs:annotation>
                      <xs:appinfo source="http://www.ogf.org/dfdl/">
                        <dfdl:setVariable ref="tns:runningVar" value="{ . }"/>
                      </xs:appinfo>
                    </xs:annotation>
                  </xs:element>
                  <xs:element name="len" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc="{ $tns:runningVar + dfdl:valueLength(../data, 'bytes') }"/>
                  <xs:element name="data" type="xs:string" dfdl:lengthKind="delimited"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val records =
      (0 until numRecords).map(i => <record>
          <idx>{f"$i%02d"}</idx>
          <data>{s"value$i"}</data>
        </record>)
    val infoset =
      <ex:root xmlns:ex={example}>
        {records}
      </ex:root>

    // Small window forces build to race many iterations ahead of write,
    // each pushing its own runningVar instance, before write catches up.
    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(
      sch,
      infoset,
      extraTunables = Map("unparsePrefetchWindowNodes" -> "2")
    )
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Simpler variant of the repeating-NVI test above: a single
  // (non-repeating) scope, as a distinct data point.
  @Test def testSingleNVIScopedVariableWithValueLengthOVCMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>, {
        <dfdl:format ref="tns:GeneralFormat"
          encoding="ascii"
          lengthUnits="bytes"/>
        <dfdl:defineVariable name="runningVar" type="xs:int"/>
      },
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="|">
            <xs:annotation>
              <xs:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:newVariableInstance ref="tns:runningVar" defaultValue="0"/>
              </xs:appinfo>
            </xs:annotation>
            <xs:element name="idx" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2">
              <xs:annotation>
                <xs:appinfo source="http://www.ogf.org/dfdl/">
                  <dfdl:setVariable ref="tns:runningVar" value="{ . }"/>
                </xs:appinfo>
              </xs:annotation>
            </xs:element>
            <xs:element name="len" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="4"
              dfdl:outputValueCalc="{ $tns:runningVar + dfdl:valueLength(../data, 'bytes') }"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="delimited"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <idx>42</idx>
        <data>hello</data>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Regression guard for NVI-scope setVariable resolution: no forward
  // OVC or separator, the minimal shape that exposes the race.
  @Test def testNVIScopedSetVariableWithNoForwardReferenceMatchesSinglePass(): Unit = {
    val numRecords = 25
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>, {
        <dfdl:format ref="tns:GeneralFormat"
          encoding="ascii"
          lengthUnits="bytes"/>
        <dfdl:defineVariable name="runningVar" type="xs:int" external="true"/>
      },
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="record" maxOccurs="unbounded">
              <xs:complexType>
                <xs:sequence>
                  <xs:annotation>
                    <xs:appinfo source="http://www.ogf.org/dfdl/">
                      <dfdl:newVariableInstance ref="tns:runningVar" defaultValue="0"/>
                    </xs:appinfo>
                  </xs:annotation>
                  <xs:element name="idx" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2">
                    <xs:annotation>
                      <xs:appinfo source="http://www.ogf.org/dfdl/">
                        <dfdl:setVariable ref="tns:runningVar" value="{ . }"/>
                      </xs:appinfo>
                    </xs:annotation>
                  </xs:element>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="summary" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="4"
              dfdl:outputValueCalc="{ $tns:runningVar }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val records = (0 until numRecords).map(i => <record><idx>{f"$i%02d"}</idx></record>)
    val infoset =
      <ex:root xmlns:ex={example}>
        {records}
      </ex:root>

    val extVars =
      ExternalVariablesLoader.mapToBindings(Map(s"{$example}runningVar" -> "-1").asJava)

    val singlePassDp = Compiler()
      .withTunable("useBuildWritePrefetch", "false")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
      .withExternalVariables(extVars)
    val singlePassBytes = TestUtils.unparseToBytes(singlePassDp, infoset)

    val prefetchDp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .withTunable("unparsePrefetchWindowNodes", "2")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
      .withExternalVariables(extVars)
    val prefetchBytes = TestUtils.unparseToBytes(prefetchDp, infoset)

    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A setup failure (inputter never produces StartDocument) must yield
  // a failed UnparseResult, not an NPE from a null error-path state.
  @Test def testMalformedInfosetInputterGetsCleanErrorNotNPE(): Unit = {
    // Needs prefetch in use (the tunable on), or DataProcessor.unparse takes
    // single-pass and unparseViaBuildThenWrite (the method under test) would
    // never run.
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]

    // hasNext() = false immediately means initialize()'s
    // "!delegate.hasNext" check fires straight away, before any actual
    // infoset event is produced; exactly the "never starts with
    // StartDocument" failure this guards.
    val neverStartsInputter = new api.infoset.InfosetInputter {
      override def getEventType() = null
      override def getLocalName() = null
      override def getNamespaceURI() = null
      override def getSimpleText(
        primType: org.apache.daffodil.runtime1.dpath.NodeInfo.Kind,
        runtimeProperties: java.util.Map[String, String]
      ) = null
      override def isNilled(): java.lang.Boolean = null
      override def hasNext() = false
      override def next(): Unit = ()
      override def fini(): Unit = ()
    }

    val out = new ByteArrayOutputStream()
    val res = dp.unparse(neverStartsInputter, out)

    assertTrue("expected a failed UnparseResult, not a successful one", res.isError)
    assertTrue(
      res.getDiagnostics.get(0).getMessage.contains("does not start with StartDocument")
    )
  }

  // A variable-length dfdl:length expression inside a separated
  // sequence must also call computeTargetLength from write's dispatch.
  @Test def testDelimitedVariableLengthExpressionMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="|">
              <xs:element name="lenField" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
              <xs:element name="content" type="xs:string"
                dfdl:lengthKind="explicit"
                dfdl:length="{ xs:int(../lenField) }"/>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <lenField>5</lenField>
        <content>hello</content>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("05|hello", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // Same dispatch path as the test above, but for a complex element
  // whose expression-based dfdl:length wraps a whole group's content chain.
  @Test def testDelimitedComplexVariableLengthExpressionMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:sequence dfdl:separator="|">
              <xs:element name="lenField" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="2"/>
              <xs:element name="wrapper" dfdl:lengthKind="explicit" dfdl:length="{ xs:int(../lenField) }">
                <xs:complexType>
                  <xs:sequence>
                    <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
                  </xs:sequence>
                </xs:complexType>
              </xs:element>
            </xs:sequence>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <lenField>3</lenField>
        <wrapper><a>xyz</a></wrapper>
      </ex:row>

    val (singlePassBytes, prefetchBytes) = TestUtils.getSinglePassAndPrefetchBytes(sch, infoset)
    assertEquals("03|xyz", new String(singlePassBytes, StandardCharsets.US_ASCII))
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A schema where every OVC is content-length-dependent (never resolvable
  // early) still uses prefetch when the tunable is on, and unparses the same.
  @Test def testPurelyContentLengthOVCSchemaStillMatchesSinglePass(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="len" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset = <ex:row xmlns:ex={example}><data>xyz</data></ex:row>

    val singlePassDp = Compiler()
      .withTunable("useBuildWritePrefetch", "false")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    val singlePassBytes = TestUtils.unparseToBytes(singlePassDp, infoset)

    val prefetchDp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertTrue(
      "prefetch is used whenever the tunable is on, whatever the OVCs",
      !prefetchDp.ssrd.builder.isEmpty
    )
    val prefetchBytes = TestUtils.unparseToBytes(prefetchDp, infoset)
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A schema mixing a resolvable OVC with an unrelated
  // content-length-dependent one must still use the prefetch path.
  @Test def testMixedSchemaWithResolvableAndContentLengthOVCStillUsesPrefetchPath(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
            <xs:element name="len" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <actual>005</actual>
        <data>xyz</data>
      </ex:row>

    val singlePassDp = Compiler()
      .withTunable("useBuildWritePrefetch", "false")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    val singlePassBytes = TestUtils.unparseToBytes(singlePassDp, infoset)

    val prefetchDp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertTrue(
      "schema has a resolvable-without-writing OVC alongside a content-length one; " +
        "must use the prefetch path",
      !prefetchDp.ssrd.builder.isEmpty
    )
    val prefetchBytes = TestUtils.unparseToBytes(prefetchDp, infoset)
    assertArrayEquals(singlePassBytes, prefetchBytes)
  }

  // A schema with no OVC at all still uses prefetch when the tunable is on.
  @Test def testSchemaWithNoOVCAtAllStillUsesPrefetch(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)
  }

  // A schema where every OVC is resolvable-without-writing uses prefetch:
  // the common case.
  @Test def testSchemaWithOnlyResolvableOVCUsesPrefetch(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)
  }

  // With the tunable explicitly off, prefetch is never used, even for a
  // schema whose OVC could resolve early.
  @Test def testTunableOffNeverUsesPrefetch(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="computed" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="3"
              dfdl:outputValueCalc="{ ../actual + 1 }"/>
            <xs:element name="actual" type="xs:int" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("useBuildWritePrefetch", "false")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertTrue(dp.ssrd.builder.isEmpty)
  }

  // Whether prefetch is in use (a non-empty builder) is baked in at compile time; confirms it survives a
  // save/reload round trip.
  @Test def testIsPrefetchInUseSurvivesSaveReload(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textNumberJustification="right"
        textNumberPadCharacter="0"
        textPadKind="padChar"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="len" type="xs:int"
              dfdl:lengthKind="explicit"
              dfdl:length="2"
              dfdl:outputValueCalc="{ dfdl:valueLength(../data, 'bytes') }"/>
            <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="3"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val dp = Compiler()
      .withTunable("useBuildWritePrefetch", "true")
      .compileNode(sch)
      .onPath("/")
      .asInstanceOf[DataProcessor]
    assertFalse(dp.ssrd.builder.isEmpty)

    val os = new ByteArrayOutputStream()
    dp.save(java.nio.channels.Channels.newChannel(os))
    val reloadedDp = Compiler()
      .reload(new java.io.ByteArrayInputStream(os.toByteArray))
      .asInstanceOf[DataProcessor]
    assertFalse(reloadedDp.ssrd.builder.isEmpty)

    val infoset = <ex:row xmlns:ex={example}><data>xyz</data></ex:row>
    assertArrayEquals(
      TestUtils.unparseToBytes(dp, infoset),
      TestUtils.unparseToBytes(reloadedDp, infoset)
    )
  }
}
