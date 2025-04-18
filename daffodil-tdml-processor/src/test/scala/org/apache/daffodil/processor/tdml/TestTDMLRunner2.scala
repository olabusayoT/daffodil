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

package org.apache.daffodil.processor.tdml

import scala.util.Using

import org.apache.daffodil.lib.Implicits._
import org.apache.daffodil.lib.xml.XMLUtils
import org.apache.daffodil.tdml.Document
import org.apache.daffodil.tdml.Runner
import org.apache.daffodil.tdml.TDMLException

import org.junit.AfterClass
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

object TestTDMLRunner2 {
  val runner = Runner("/test/tdml/", "tdmlQuoting.tdml")

  @AfterClass def shutDown(): Unit = {
    runner.reset()
  }
}

class TestTDMLRunner2 {

  import TestTDMLRunner2._

  val tdml = XMLUtils.TDML_NAMESPACE
  val dfdl = XMLUtils.DFDL_NAMESPACE
  val xsi = XMLUtils.XSI_NAMESPACE
  val xsd = XMLUtils.XSD_NAMESPACE
  val example = XMLUtils.EXAMPLE_NAMESPACE
  val tns = example
  // val sub = XMLUtils.DFDL_XMLSCHEMASUBSET_NAMESPACE

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? Yes
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Demonstrates that when validation is off, no validation errors will be
   * generated and so the test will fail because it expects them.
   *
   * Note that this test case is not considered invalid because there are ways
   * to generate validation errors with validation being off, such as asserts
   * with failureType="recoverableError"
   */
  @Test def testValidationOffValidationErrorGivenShouldError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </tns:array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
          <tdml:validationErrors>
            <tdml:error>This error will not be found with validation disabled</tdml:error>
          </tdml:validationErrors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("testValidation")
    }
    runner.reset()
    val msg = e.getMessage()
    assertTrue(msg.contains("expected but not found"))
  }

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? No
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Helps demonstrate the optionality of including the validation errors
   * in the testcase. <verrors/> means no validation errors are expected.
   */
  @Test def testValidationOffValidationErrorGivenButEmptyNotError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </tns:array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testValidation")
    runner.reset()
  }

  /**
   * Validation=Off
   * Should Parse Succeed? Yes
   * Exception expected? No
   *
   * Reasoning: The data parses successfully and validation is 'off'.
   * Helps demonstrate the optionality of including the validation errors
   * in the testcase.
   */
  @Test def testValidationOffValidationErrorNotGivenNotError() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testValidation" root="array" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </tns:array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testValidation")
    runner.reset()
  }

  /**
   * Validation=Limited
   * Should Parse Succeed? Yes
   * Exception expected? Yes
   *
   * Reasoning: The data parses successfully and fails 'limited' validation.
   * However the test case itself does not expect a validation error. Since
   * ignoreUnexpectedValidationError is set to false, the test will thrown an
   * Exception alerting the test writer to the fact that a validation occurred
   * that was not 'captured' in the test case.
   */
  @Test def testValidationLimitedValidationErrorNotCapturedShouldThrow() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={
        tdml
      } name="testValidation" root="array" model="mySchema" validation="limited" ignoreUnexpectedValidationErrors="false">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </tns:array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    val e = intercept[Exception] {
      runner.runOneTest("testValidation")
    }
    runner.reset()
    val msg = e.getMessage()
    assertTrue(
      msg.contains(
        "ignoreUnexpectedValidationErrors = false and test does not expect ValidationError diagnostics"
      )
    )
  }

  /**
   * Validation=Limited
   * Should Parse Succeed? Yes
   * Exception expected? No
   *
   * Reasoning: The data parses successfully and fails 'limited' validation.
   * However the test case itself does not expect a validation error.  Since
   * the ignoreUnexpectedValidationError defaults to true, the unexpected validation
   * error will be ignored, and the test will poss
   */
  @Test def testValidationLimitedValidationErrorNotCaptured = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat" initiator="" terminator="" leadingSkip="0" trailingSkip="0" textBidi="no" floating="no" encoding="utf-8" byteOrder="bigEndian" alignment="1" alignmentUnits="bytes" fillByte="f" occursCountKind="parsed" truncateSpecifiedLengthString="no" ignoreCase="no" representation="text" lengthKind="delimited" nilValueDelimiterPolicy="both" emptyValueDelimiterPolicy="none" documentFinalTerminatorCanBeMissing="yes" initiatedContent="no" separatorSuppressionPolicy="anyEmpty" separatorPosition="infix"/>
          <xsd:element name="array" type="tns:arrayType" dfdl:lengthKind="implicit"/>
          <xsd:complexType name="arrayType">
            <xsd:sequence dfdl:separator="|">
              <xsd:element name="data" type="xsd:int" minOccurs="2" maxOccurs="5" dfdl:textNumberRep="standard" dfdl:lengthKind="delimited"/>
            </xsd:sequence>
          </xsd:complexType>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={
        tdml
      } name="testValidation" root="array" model="mySchema" validation="limited">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[1|2|3|4|5|6|7|8|9]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:array>
                <data>1</data>
                <data>2</data>
                <data>3</data>
                <data>4</data>
                <data>5</data>
                <data>6</data>
                <data>7</data>
                <data>8</data>
                <data>9</data>
              </tns:array>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testValidation")
  }

  /**
   * Scala's XML Literals don't do CDATA regions right.
   *
   * So to force the example tdml xml to have CDATA regions (which it would
   * if these were being read from a file), we construct actual PCData
   * xml nodes and splice them in where we would have written <![CDATA[...]]>
   * in a real TDML file.
   */
  val cdataText = """(?x) # free form
abc # a comment
# a line with only a comment
123 # another comment
"""
  val cdata = new scala.xml.PCData(cdataText)

  /**
   * Test to make sure we can use freeform regex and also use the comment syntax.
   */
  @Test def testRegexWithFreeFormAndComments1() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="pattern" dfdl:terminator="abcdef">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <dfdl:element>
                  <dfdl:property name="lengthPattern">{cdata}</dfdl:property>
                </dfdl:element>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data/>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testRegex")
    runner.reset()
  }

  @Test def testRegexWithFreeFormAndComments2() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <!-- This assert passes only if free form works, and comments work. -->
                <dfdl:assert testKind='pattern'>{cdata}</dfdl:assert>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abc123]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data>abc123</tns:data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testRegex")
    runner.reset()
  }

  /**
   * Scala's xml literals read CDATA properly, but don't create a PCData node
   * so if you write the data back out, the CDATA-non-normalized whitespace
   * is lost. So in this test we forcibly construct the PCData node.
   *
   */
  @Test def testRegexWithFreeFormAndComments3() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <!-- This assert passes only if free form works, and comments work. -->
                <dfdl:assert testKind='pattern'>{cdata}</dfdl:assert>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abc123]]></tdml:documentPart>
          </tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <tns:data>abc123</tns:data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testRegex")
    runner.reset()
  }

  @Test def testRegexWithFreeFormAndComments4() = {
    val cdataText = """(?x) # free form
abc # a comment
# a line with only a comment
123 # another comment
""".replaceAll("\r\n", "\n")
    val cdata = new scala.xml.PCData(cdataText)
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <dfdl:format ref="tns:GeneralFormat"/>
          <xsd:element name="data" type="xsd:string" dfdl:lengthKind="delimited">
            <xsd:annotation>
              <xsd:appinfo source="http://www.ogf.org/dfdl/">
                <!-- This assert passes only if free form works, and comments work. -->
                <dfdl:assert testKind='pattern'>{cdata}</dfdl:assert>
              </xsd:appinfo>
            </xsd:annotation>
          </xsd:element>
        </tdml:defineSchema>
        <tdml:parserTestCase xmlns={tdml} name="testRegex" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>{cdataText.trim}</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testRegex")
    runner.reset()
  }

  @Test def testComplexDocument() = {
    val doc = <tdml:document>
                <tdml:documentPart type="bits">00101010</tdml:documentPart>
                <tdml:documentPart type="bits">1</tdml:documentPart>
                <tdml:documentPart type="bits">00000000 00000000 00000000 00000001</tdml:documentPart>
                <tdml:documentPart type="bits">0</tdml:documentPart>
                <tdml:documentPart type="bits">1</tdml:documentPart>
              </tdml:document>
    val docObj = new Document(doc, null)
    val bits = docObj.documentBits
    val expected = List("00101010", "10000000", "00000000", "00000000", "00000000", "10100000")
    assertEquals(expected, bits)
  }

  @Test def testTDMLWithInvalidDFDLSchemaEmbedded() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xs={xsd}>
        <!-- This embedded schema has validation errors. -->
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:format ref="tns:GeneralFormat" />
          <xs:element name="data" type="fn:string" dfdl:lengthKind="delimited" />
        </tdml:defineSchema>
        <tdml:parserTestCase name="test1" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>resolve</tdml:error>
            <tdml:error>fn:string</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite, validateTDMLFile = false)
    runner.runOneTest("test1")
    runner.reset()
  }

  @Test def testTDMLUnparse1(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={tdml} xmlns:tns={tns} xmlns:dfdl={dfdl} xmlns:xs={
      xsd
    } xmlns:xsi={xsi} suiteName="theSuiteName">
                      <ts:defineSchema name="unparseTestSchema1">
                        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 9 }"/>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="some identifier" name="testTDMLUnparse" root="data" model="unparseTestSchema1">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={example}>123456789</data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:document>123456789</ts:document>
                      </ts:unparserTestCase>
                    </ts:testSuite>

    val runner = Runner(testSuite)
    runner.runOneTest("testTDMLUnparse")
    runner.reset()
  }

  @Test def testTDMLUnparse2(): Unit = {
    val testSuite = <ts:testSuite xmlns:ts={tdml} xmlns:tns={tns} xmlns:dfdl={dfdl} xmlns:xs={
      xsd
    } xmlns:xsi={xsi} suiteName="theSuiteName">
                      <ts:defineSchema name="unparseTestSchema1">
                        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
                        <dfdl:format ref="tns:GeneralFormat"/>
                        <xs:element name="data">
                          <xs:complexType>
                            <xs:sequence>
                              <xs:element name="e1" type="xs:string" dfdl:lengthKind="explicit" dfdl:length="{ 9 }"/>
                              <xs:element name="len" type="xs:string" dfdl:inputValueCalc="{ fn:string-length(../e1)}"/>
                            </xs:sequence>
                          </xs:complexType>
                        </xs:element>
                      </ts:defineSchema>
                      <ts:unparserTestCase ID="some identifier" name="testTDMLUnparse" root="data" model="unparseTestSchema1">
                        <ts:infoset>
                          <ts:dfdlInfoset>
                            <data xmlns={example}>
                              <e1>123456789</e1>
                              <len/>
                            </data>
                          </ts:dfdlInfoset>
                        </ts:infoset>
                        <ts:warnings>
                          <ts:warning/>
                        </ts:warnings>
                      </ts:unparserTestCase>
                    </ts:testSuite>

    val runner = Runner(testSuite)
    val err = intercept[TDMLException] {
      runner.runOneTest("testTDMLUnparse")
    }
    val actualMessage = err.getMessage()
    assertTrue(actualMessage.contains("Either tdml:document or tdml:errors must be present"))
    runner.reset()
  }

  @Test def test_quote_test1() = {
    val runner = Runner("/test/tdml/", "tdmlQuoting.tdml")
    runner.runOneTest("quote_test1")
    runner.reset()
  }

  val tdmlWithEmbeddedSchema =
    <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
      dfdl
    } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
      <tdml:defineSchema name="mySchema">
        <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
        <dfdl:format ref="tns:GeneralFormat"/>
        <xsd:element name="data" type="xsd:int" dfdl:lengthKind="explicit" dfdl:length="{ xs:unsignedInt(2) }"/>
      </tdml:defineSchema>
      <parserTestCase xmlns={tdml} name="testEmbeddedSchemaWorks" root="data" model="mySchema">
        <document>37</document>
        <infoset>
          <dfdlInfoset>
            <data xmlns={example}>37</data>
          </dfdlInfoset>
        </infoset>
      </parserTestCase>
    </tdml:testSuite>

  // @Test // other places should test tracing.
  def testTrace(): Unit = {
    val tmpTDMLFileName = getClass.getName() + ".tdml"
    val testSuite = tdmlWithEmbeddedSchema
    try {
      Using.resource(new java.io.FileWriter(tmpTDMLFileName)) { fw =>
        fw.write(testSuite.toString())
      }
      val runner = new Runner(new java.io.File(tmpTDMLFileName))
      runner.trace
      runner.runAllTests()
      runner.reset()
    } finally {
      val t = new java.io.File(tmpTDMLFileName)
      t.delete()
    }
  }

  /**
   * Tests illustrate tdml runner correctly processes apostrophes (')
   * in the html format (&apos;) within the document or infoset data.
   */
  @Test def test_apos_test1(): Unit = { runner.runOneTest("apos_test1") }
  @Test def test_apos_test2(): Unit = { runner.runOneTest("apos_test2") }

  // DFDL-2955
  @Test def test_whitespaceSurroundingErrorMessages() = {
    val testSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:tns={tns} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xs={xsd}>
        <!-- This embedded schema has validation errors. -->
        <tdml:defineSchema name="mySchema">
          <xs:include schemaLocation="/org/apache/daffodil/xsd/DFDLGeneralFormat.dfdl.xsd"/>
          <xs:format ref="tns:GeneralFormat" />
          <xs:element name="data" type="fn:string" dfdl:lengthKind="delimited" />
        </tdml:defineSchema>
        <tdml:parserTestCase name="test1" root="data" model="mySchema">
          <tdml:document>
            <tdml:documentPart type="text"><![CDATA[abcdef]]></tdml:documentPart>
          </tdml:document>
          <tdml:errors>
            <tdml:error>   cannot resolve   </tdml:error>
            <tdml:error>  fn:string </tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = Runner(testSuite, validateTDMLFile = false)
    runner.runOneTest("test1")
    runner.reset()
  }

  // DFDL-2947
  @Test def test_rootNSSpecifiesNoNamespaceRoot(): Unit = {
    val tdmlTestSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:ex={example} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:parserTestCase name="test1" root="data" rootNS="" model="/test/tdml/chameleon-schema1.dfdl.xsd">
          <tdml:document>37</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <data>37</data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(tdmlTestSuite)
    runner.runOneTest("test1")
  }

  @Test def test_rootNSSpecifiesNamespaceRoot(): Unit = {
    val tdmlTestSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:ex={example} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:parserTestCase name="test1" root="data" rootNS="urn:schema:namespace" model="/test/tdml/chameleon-schema1.dfdl.xsd">
          <tdml:document>boy</tdml:document>
          <tdml:infoset>
            <tdml:dfdlInfoset>
              <data>boy</data>
            </tdml:dfdlInfoset>
          </tdml:infoset>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(tdmlTestSuite)
    runner.runOneTest("test1")
  }

  @Test def test_noRootCandidates1(): Unit = {
    val tdmlTestSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:ex={example} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:parserTestCase name="test1" root="data1" model="/test/tdml/chameleon-schema1.dfdl.xsd">
          <tdml:document>37</tdml:document>
          <tdml:errors>
          <tdml:error>Schema Definition Error</tdml:error>
          <tdml:error>no root element found</tdml:error>
          <tdml:error>data1</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(tdmlTestSuite)
    runner.runOneTest("test1")
  }

  @Test def test_noRootCandidates2(): Unit = {
    val tdmlTestSuite =
      <tdml:testSuite suiteName="theSuiteName" xmlns:ex={example} xmlns:tdml={tdml} xmlns:dfdl={
        dfdl
      } xmlns:xsd={xsd} xmlns:xs={xsd} xmlns:xsi={xsi}>
        <tdml:parserTestCase name="test1" root="data" rootNS="doesNotExist" model="/test/tdml/chameleon-schema1.dfdl.xsd">
          <tdml:document>37</tdml:document>
          <tdml:errors>
          <tdml:error>Schema Definition Error</tdml:error>
          <tdml:error>no global element found</tdml:error>
            <!-- extra brace is there escape the inner brace -->
          <tdml:error>{{doesNotExist}}data</tdml:error>
          </tdml:errors>
        </tdml:parserTestCase>
      </tdml:testSuite>

    val runner = new Runner(tdmlTestSuite)
    runner.runOneTest("test1")
  }
}
