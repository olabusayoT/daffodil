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

import org.apache.daffodil.core.compiler.Compiler
import org.apache.daffodil.lib.util.SchemaUtils
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.runtime1.infoset.ScalaXMLInfosetInputter

import org.junit.Assert.*
import org.junit.Test

class TestBuildThenWriteUnparse {

  private val example = XMLUtils.EXAMPLE_NAMESPACE

  private def compile(
    schema: scala.xml.Node,
    extraTunables: Map[String, String] = Map.empty
  ): DataProcessor = {
    val pf = Compiler()
      .withTunable("useBuildThenWrite", "true")
      .withTunables(extraTunables)
      .compileNode(schema)
    if (pf.isError) fail(pf.getDiagnostics.toString)
    val dp = pf.onPath("/").asInstanceOf[DataProcessor]
    if (dp.isError) fail(dp.getDiagnostics.toString)
    dp
  }

  private def unparse(dp: DataProcessor, infoset: scala.xml.Node): String = {
    val out = new ByteArrayOutputStream()
    val res = dp.unparse(new ScalaXMLInfosetInputter(infoset), out)
    if (res.isProcessingError) fail(res.getDiagnostics.toString)
    new String(out.toByteArray, StandardCharsets.US_ASCII)
  }

  @Test def testScalarFields(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
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
      <ex:row xmlns:ex={example}>
        <name>Alice</name>
        <age>30</age>
        <city>Boston</city>
      </ex:row>
    val dp = compile(sch)
    assertEquals("Alice,30,Boston", unparse(dp, infoset))
  }

  @Test def testArrayAndChoice(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
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
    val dp = compile(sch)
    assertEquals("H,a,b,c,X", unparse(dp, infoset))
  }

  @Test def testOvcReadsForwardOvc(): Unit = {
    val N = 3
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        textPadKind="padChar"
        textStringJustification="left"
        textStringPadCharacter="%SP;"/>,
      <xs:element name="root" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
            <xs:element name="header" dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="lenA" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc="{ ../lenB }"/>
                  <xs:element name="lenB" type="xs:int"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"
                    dfdl:outputValueCalc={
        s"{ dfdl:valueLength(../../record[$N]/data, 'bytes') }"
      }/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
            <xs:element name="record" minOccurs={N.toString} maxOccurs={N.toString}
              dfdl:lengthKind="implicit">
              <xs:complexType>
                <xs:sequence dfdl:separator="" dfdl:sequenceKind="ordered">
                  <xs:element name="tag" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="4"/>
                  <xs:element name="data" type="xs:string"
                    dfdl:lengthKind="explicit"
                    dfdl:length="8"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val recordXml = (1 to N).map { i =>
      <record>
        <tag>{f"T$i%03d"}</tag>
        <data>{f"data$i%04d"}</data>
      </record>
    }
    val infoset =
      <ex:root xmlns:ex={example}>
        <header/>
        {recordXml}
      </ex:root>
    val dp = compile(sch, Map("unparseSuspensionWaitOld" -> "1000000"))
    val unparsed = unparse(dp, infoset)
    assertEquals("   8   8", unparsed.substring(0, 8))
    val expectedRecords = (1 to N).map(i => f"T$i%03d" + f"data$i%04d").mkString
    assertEquals(expectedRecords, unparsed.substring(8))
  }

  @Test def testPrefixedLength(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        representation="text"
        textNumberRep="standard"/>,
      <xs:simpleType name="strLen">
        <xs:annotation>
          <xs:appinfo source="http://www.ogf.org/dfdl/">
            <dfdl:simpleType lengthKind="explicit" length="2" representation="text"/>
          </xs:appinfo>
        </xs:annotation>
        <xs:restriction base="xs:unsignedByte"/>
      </xs:simpleType>
      <xs:simpleType name="pString" dfdl:lengthKind="prefixed"
        dfdl:prefixLengthType="tns:strLen"
        dfdl:prefixIncludesPrefixLength="no">
        <xs:restriction base="xs:string"/>
      </xs:simpleType>
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="b" type="tns:pString"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <a>X</a>
        <b>hello</b>
      </ex:row>
    val dp = compile(sch)
    assertEquals("X,5 hello", unparse(dp, infoset))
  }

  @Test def testRepType(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        representation="binary"/>,
      <xs:simpleType name="uint8" dfdl:lengthKind="explicit" dfdl:length="1">
        <xs:restriction base="xs:unsignedInt"/>
      </xs:simpleType>
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit"
              dfdl:length="1" dfdl:representation="text"/>
            <xs:element name="color" dfdlx:repType="tns:uint8">
              <xs:simpleType>
                <xs:restriction base="xs:string">
                  <xs:enumeration value="red" dfdlx:repValues="1"/>
                  <xs:enumeration value="green" dfdlx:repValues="2"/>
                </xs:restriction>
              </xs:simpleType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <a>X</a>
        <color>green</color>
      </ex:row>
    val dp = compile(sch)
    assertEquals("X" + 2.toChar, unparse(dp, infoset))
  }

  @Test def testHiddenGroup(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat" encoding="ascii" lengthUnits="bytes"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence>
            <xs:element name="visible" type="xs:string" dfdl:lengthKind="explicit"
              dfdl:length="1"/>
            <xs:sequence dfdl:hiddenGroupRef="ex:hg"/>
          </xs:sequence>
        </xs:complexType>
      </xs:element>
      <xs:group name="hg">
        <xs:sequence>
          <xs:element name="hidden" type="xs:string" dfdl:lengthKind="explicit"
            dfdl:length="1" dfdl:outputValueCalc="{ 'z' }"/>
        </xs:sequence>
      </xs:group>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <visible>A</visible>
      </ex:row>
    val dp = compile(sch)
    assertEquals("Az", unparse(dp, infoset))
  }

  @Test def testNilledComplexElement(): Unit = {
    val sch = SchemaUtils.dfdlTestSchema(
      <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>,
      <dfdl:format ref="tns:GeneralFormat"
        encoding="ascii"
        lengthUnits="bytes"
        nilValueDelimiterPolicy="both"
        representation="text"/>,
      <xs:element name="row" dfdl:lengthKind="implicit">
        <xs:complexType>
          <xs:sequence dfdl:separator="," dfdl:separatorPosition="infix">
            <xs:element name="a" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="1"/>
            <xs:element name="b" nillable="true" dfdl:lengthKind="delimited"
              dfdl:nilKind="literalValue"
              dfdl:nilValue="%ES;"
              dfdl:nilValueDelimiterPolicy="both">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="c" type="xs:string" dfdl:lengthKind="explicit"
                    dfdl:length="1"/>
                </xs:sequence>
              </xs:complexType>
            </xs:element>
          </xs:sequence>
        </xs:complexType>
      </xs:element>,
      elementFormDefault = "unqualified"
    )
    val infoset =
      <ex:row xmlns:ex={example}>
        <a>X</a>
        <b xsi:nil="true" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"/>
      </ex:row>
    val dp = compile(sch)
    assertEquals("X,", unparse(dp, infoset))
  }
}
