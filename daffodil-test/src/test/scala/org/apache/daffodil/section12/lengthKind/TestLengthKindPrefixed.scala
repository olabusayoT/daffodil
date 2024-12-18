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

import org.apache.daffodil.tdml.Runner

import org.junit.AfterClass
import org.junit.Test

object TestLengthKindPrefixed {
  private val testDir = "/org/apache/daffodil/section12/lengthKind/"

  val runner =
    Runner(testDir, "PrefixedTests.tdml", validateTDMLFile = false, validateDFDLSchemas = false)

  @AfterClass def shutDown(): Unit = {
    runner.reset
  }

}

class TestLengthKindPrefixed {

  import TestLengthKindPrefixed._

  @Test def test_pl_text_string_txt_bytes() = { runner.runOneTest("pl_text_string_txt_bytes") }
  @Test def test_pl_text_string_txt_bits() = { runner.runOneTest("pl_text_string_txt_bits") }
  @Test def test_pl_text_string_txt_chars() = { runner.runOneTest("pl_text_string_txt_chars") }
  @Test def test_pl_text_string_txt_bytes_includes() = {
    runner.runOneTest("pl_text_string_txt_bytes_includes")
  }
  @Test def test_pl_text_string_txt_bits_includes() = {
    runner.runOneTest("pl_text_string_txt_bits_includes")
  }
  @Test def test_pl_text_string_txt_chars_includes() = {
    runner.runOneTest("pl_text_string_txt_chars_includes")
  }
  @Test def test_pl_text_string_txt_chars_padding() = {
    runner.runOneTest("pl_text_string_txt_chars_padding")
  }
  @Test def test_pl_text_string_bin_bytes() = { runner.runOneTest("pl_text_string_bin_bytes") }
  @Test def test_pl_text_string_bin_bits() = { runner.runOneTest("pl_text_string_bin_bits") }
  @Test def test_pl_text_string_txt_bytes_nil() = {
    runner.runOneTest("pl_text_string_txt_bytes_nil")
  }
  @Test def test_pl_text_string_txt_bits_nil() = {
    runner.runOneTest("pl_text_string_txt_bits_nil")
  }
  @Test def test_pl_text_string_txt_chars_nil() = {
    runner.runOneTest("pl_text_string_txt_chars_nil")
  }
  @Test def test_pl_text_string_bin_bytes_nil() = {
    runner.runOneTest("pl_text_string_bin_bytes_nil")
  }
  @Test def test_pl_text_string_bin_bits_nil() = {
    runner.runOneTest("pl_text_string_bin_bits_nil")
  }
  @Test def test_pl_text_string_txt_bytes_neg_len() = {
    runner.runOneTest("pl_text_string_txt_bytes_neg_len")
  }
  @Test def test_pl_text_string_txt_bytes_not_enough_data() = {
    runner.runOneTest("pl_text_string_txt_bytes_not_enough_data")
  }
  @Test def test_pl_text_string_txt_bytes_not_enough_prefix_data() = {
    runner.runOneTest("pl_text_string_txt_bytes_not_enough_prefix_data")
  }
  @Test def test_pl_text_string_txt_bytes_not_enough_prefix_data_includes_backtrack() = {
    runner.runOneTest("pl_text_string_txt_bytes_not_enough_prefix_data_includes_backtrack")
  }
  // DFDL-2660
  @Test def test_pl_check_prefix_facets_before_use1() = {
    runner.runOneTest("pl_check_prefix_facets_before_use1")
  }
  @Test def test_pl_check_prefix_facets_before_use2() = {
    runner.runOneTest("pl_check_prefix_facets_before_use2")
  }
  @Test def test_pl_check_prefix_facets_before_use3() = {
    runner.runOneTest("pl_check_prefix_facets_before_use3")
  }
  @Test def test_pl_check_prefix_facets_before_use4() = {
    runner.runOneTest("pl_check_prefix_facets_before_use4")
  }
  @Test def test_pl_check_prefix_facets_before_use5() = {
    runner.runOneTest("pl_check_prefix_facets_before_use5")
  }
  @Test def test_pl_check_prefix_facets_before_use6() = {
    runner.runOneTest("pl_check_prefix_facets_before_use6")
  }
  @Test def test_pl_check_prefix_facets_before_use7() = {
    runner.runOneTest("pl_check_prefix_facets_before_use7")
  }
  @Test def test_pl_check_prefix_facets_before_use8() = {
    runner.runOneTest("pl_check_prefix_facets_before_use8")
  }
  @Test def test_pl_check_prefix_facets_before_use9() = {
    runner.runOneTest("pl_check_prefix_facets_before_use9")
  }
  @Test def test_pl_check_prefix_for_annotations() = {
    runner.runOneTest("pl_check_prefix_for_annotations")
  }
  // DFDL-2030, nested prefixed lengths not supported
  // @Test def test_pl_text_string_pl_txt_bytes() = { runner.runOneTest("pl_text_string_pl_txt_bytes") }
  @Test def test_pl_text_int_txt_bytes() = { runner.runOneTest("pl_text_int_txt_bytes") }
  @Test def test_pl_text_int_txt_bits() = { runner.runOneTest("pl_text_int_txt_bits") }
  @Test def test_pl_text_int_txt_chars() = { runner.runOneTest("pl_text_int_txt_chars") }
  @Test def test_pl_text_int_bin_bytes() = { runner.runOneTest("pl_text_int_bin_bytes") }
  @Test def test_pl_text_int_bin_bits() = { runner.runOneTest("pl_text_int_bin_bits") }
  @Test def test_pl_text_int_txt_bytes_includes() = {
    runner.runOneTest("pl_text_int_txt_bytes_includes")
  }
  @Test def test_pl_text_int_txt_bits_includes() = {
    runner.runOneTest("pl_text_int_txt_bits_includes")
  }
  @Test def test_pl_text_int_txt_chars_includes() = {
    runner.runOneTest("pl_text_int_txt_chars_includes")
  }
  @Test def test_pl_text_int_bin_bytes_includes() = {
    runner.runOneTest("pl_text_int_bin_bytes_includes")
  }
  @Test def test_pl_text_int_bin_bits_includes() = {
    runner.runOneTest("pl_text_int_bin_bits_includes")
  }
  @Test def test_pl_text_dec_txt_bytes() = { runner.runOneTest("pl_text_dec_txt_bytes") }
  @Test def test_pl_text_dec_txt_bits() = { runner.runOneTest("pl_text_dec_txt_bits") }
  @Test def test_pl_text_dec_txt_chars() = { runner.runOneTest("pl_text_dec_txt_chars") }
  @Test def test_pl_text_dec_bin_bytes() = { runner.runOneTest("pl_text_dec_bin_bytes") }
  @Test def test_pl_text_dec_bin_bits() = { runner.runOneTest("pl_text_dec_bin_bits") }
  @Test def test_pl_text_date_txt_bytes() = { runner.runOneTest("pl_text_date_txt_bytes") }
  @Test def test_pl_text_date_txt_bits() = { runner.runOneTest("pl_text_date_txt_bits") }
  @Test def test_pl_text_date_txt_chars() = { runner.runOneTest("pl_text_date_txt_chars") }
  @Test def test_pl_text_date_bin_bytes() = { runner.runOneTest("pl_text_date_bin_bytes") }
  @Test def test_pl_text_date_bin_bits() = { runner.runOneTest("pl_text_date_bin_bits") }
  @Test def test_pl_text_bool_txt_bytes() = { runner.runOneTest("pl_text_bool_txt_bytes") }
  @Test def test_pl_text_bool_txt_bits() = { runner.runOneTest("pl_text_bool_txt_bits") }
  @Test def test_pl_text_bool_txt_chars() = { runner.runOneTest("pl_text_bool_txt_chars") }
  @Test def test_pl_text_bool_bin_bytes() = { runner.runOneTest("pl_text_bool_bin_bytes") }
  @Test def test_pl_text_bool_bin_bits() = { runner.runOneTest("pl_text_bool_bin_bits") }

  @Test def test_pl_text_int_txt_bytes_plbits() = {
    runner.runOneTest("pl_text_int_txt_bytes_plbits")
  }
  @Test def test_pl_text_int_txt_bytes_plchars() = {
    runner.runOneTest("pl_text_int_txt_bytes_plchars")
  }
  @Test def test_pl_text_int_txt_bits_plbytes() = {
    runner.runOneTest("pl_text_int_txt_bits_plbytes")
  }
  @Test def test_pl_text_int_txt_bits_plchars() = {
    runner.runOneTest("pl_text_int_txt_bits_plchars")
  }
  @Test def test_pl_text_int_txt_chars_plbits() = {
    runner.runOneTest("pl_text_int_txt_chars_plbits")
  }
  @Test def test_pl_text_int_txt_chars_plbytes() = {
    runner.runOneTest("pl_text_int_txt_chars_plbytes")
  }
  @Test def test_pl_text_int_bin_bytes_plbits() = {
    runner.runOneTest("pl_text_int_bin_bytes_plbits")
  }
  @Test def test_pl_text_int_bin_bytes_plchars() = {
    runner.runOneTest("pl_text_int_bin_bytes_plchars")
  }
  @Test def test_pl_text_int_bin_bits_plbytes() = {
    runner.runOneTest("pl_text_int_bin_bits_plbytes")
  }
  @Test def test_pl_text_int_bin_bits_plchars() = {
    runner.runOneTest("pl_text_int_bin_bits_plchars")
  }

  @Test def test_pl_complex_bin_bytes() = { runner.runOneTest("pl_complex_bin_bytes") }
  @Test def test_pl_complex_bin_bits() = { runner.runOneTest("pl_complex_bin_bits") }
  @Test def test_pl_complex_bin_bytes_suspension() = {
    runner.runOneTest("pl_complex_bin_bytes_suspension")
  }
  @Test def test_pl_complex_bin_bytes_suspension_includes() = {
    runner.runOneTest("pl_complex_bin_bytes_suspension_includes")
  }
  @Test def test_pl_bin_int_txt_bytes() = { runner.runOneTest("pl_bin_int_txt_bytes") }
  @Test def test_pl_bin_int_txt_bits() = { runner.runOneTest("pl_bin_int_txt_bits") }
  @Test def test_pl_bin_int_bin_bytes() = { runner.runOneTest("pl_bin_int_bin_bytes") }
  @Test def test_pl_bin_int_bin_bits() = { runner.runOneTest("pl_bin_int_bin_bits") }
  @Test def test_pl_bin_int_txt_bytes_includes() = {
    runner.runOneTest("pl_bin_int_txt_bytes_includes")
  }
  @Test def test_pl_bin_int_txt_bits_includes() = {
    runner.runOneTest("pl_bin_int_txt_bits_includes")
  }
  @Test def test_pl_bin_int_bin_bytes_includes() = {
    runner.runOneTest("pl_bin_int_bin_bytes_includes")
  }
  @Test def test_pl_bin_int_bin_bits_includes() = {
    runner.runOneTest("pl_bin_int_bin_bits_includes")
  }
  @Test def test_pl_bin_int_bin_bytes_packed() = {
    runner.runOneTest("pl_bin_int_bin_bytes_packed")
  }
  @Test def test_pl_bin_int_bin_bits_packed() = {
    runner.runOneTest("pl_bin_int_bin_bits_packed")
  }
  @Test def test_pl_bin_int_bin_bytes_bcd() = { runner.runOneTest("pl_bin_int_bin_bytes_bcd") }
  @Test def test_pl_bin_int_bin_bits_bcd() = { runner.runOneTest("pl_bin_int_bin_bits_bcd") }
  @Test def test_pl_bin_int_bin_bytes_ibm4690() = {
    runner.runOneTest("pl_bin_int_bin_bytes_ibm4690")
  }
  @Test def test_pl_bin_int_bin_bits_ibm4690() = {
    runner.runOneTest("pl_bin_int_bin_bits_ibm4690")
  }
  @Test def test_pl_bin_dec_txt_bytes() = { runner.runOneTest("pl_bin_dec_txt_bytes") }
  @Test def test_pl_bin_dec_txt_bits() = { runner.runOneTest("pl_bin_dec_txt_bits") }
  @Test def test_pl_bin_dec_bin_bytes() = { runner.runOneTest("pl_bin_dec_bin_bytes") }
  @Test def test_pl_bin_dec_bin_bits() = { runner.runOneTest("pl_bin_dec_bin_bits") }
  @Test def test_pl_bin_dec_bin_bytes_packed() = {
    runner.runOneTest("pl_bin_dec_bin_bytes_packed")
  }
  @Test def test_pl_bin_dec_bin_bits_packed() = {
    runner.runOneTest("pl_bin_dec_bin_bits_packed")
  }
  @Test def test_pl_bin_dec_bin_bytes_bcd() = { runner.runOneTest("pl_bin_dec_bin_bytes_bcd") }
  @Test def test_pl_bin_dec_bin_bits_bcd() = { runner.runOneTest("pl_bin_dec_bin_bits_bcd") }
  @Test def test_pl_bin_dec_bin_bytes_ibm4690() = {
    runner.runOneTest("pl_bin_dec_bin_bytes_ibm4690")
  }
  @Test def test_pl_bin_dec_bin_bits_ibm4690() = {
    runner.runOneTest("pl_bin_dec_bin_bits_ibm4690")
  }
  @Test def test_pl_bin_hex_txt_bytes() = { runner.runOneTest("pl_bin_hex_txt_bytes") }
  @Test def test_pl_bin_hex_txt_bits() = { runner.runOneTest("pl_bin_hex_txt_bits") }
  @Test def test_pl_bin_hex_bin_bytes() = { runner.runOneTest("pl_bin_hex_bin_bytes") }
  @Test def test_pl_bin_hex_bin_bits() = { runner.runOneTest("pl_bin_hex_bin_bits") }
  @Test def test_pl_bin_bool_txt_bytes() = { runner.runOneTest("pl_bin_bool_txt_bytes") }
  @Test def test_pl_bin_bool_txt_bits() = { runner.runOneTest("pl_bin_bool_txt_bits") }
  @Test def test_pl_bin_bool_bin_bytes() = { runner.runOneTest("pl_bin_bool_bin_bytes") }
  @Test def test_pl_bin_bool_bin_bits() = { runner.runOneTest("pl_bin_bool_bin_bits") }
  @Test def test_pl_bin_date_bin_bytes_packed() = {
    runner.runOneTest("pl_bin_date_bin_bytes_packed")
  }
  @Test def test_pl_bin_date_bin_bits_packed() = {
    runner.runOneTest("pl_bin_date_bin_bits_packed")
  }
  @Test def test_pl_bin_date_bin_bytes_bcd() = {
    runner.runOneTest("pl_bin_date_bin_bytes_bcd")
  }
  @Test def test_pl_bin_date_bin_bits_bcd() = { runner.runOneTest("pl_bin_date_bin_bits_bcd") }
  @Test def test_pl_bin_date_bin_bytes_ibm4690() = {
    runner.runOneTest("pl_bin_date_bin_bytes_ibm4690")
  }
  @Test def test_pl_bin_date_bin_bits_ibm4690() = {
    runner.runOneTest("pl_bin_date_bin_bits_ibm4690")
  }
  @Test def test_plSlash1_data() = { runner.runOneTest("plSlash1_data") }

  @Test def test_pl_complex_err() = { runner.runOneTest("pl_complex_err") }
  @Test def test_pl_delimited_err() = { runner.runOneTest("pl_delimited_err") }
  @Test def test_pl_endofparent_err() = { runner.runOneTest("pl_endofparent_err") }
  @Test def test_pl_pattern_err() = { runner.runOneTest("pl_pattern_err") }
  @Test def test_pl_expression_err() = { runner.runOneTest("pl_expression_err") }
  @Test def test_pl_ovc_err() = { runner.runOneTest("pl_ovc_err") }
  @Test def test_pl_initiator_err() = { runner.runOneTest("pl_initiator_err") }
  @Test def test_pl_terminator_err() = { runner.runOneTest("pl_terminator_err") }
  @Test def test_pl_alignment_err() = { runner.runOneTest("pl_alignment_err") }
  @Test def test_pl_leadingskip_err() = { runner.runOneTest("pl_leadingskip_err") }
  @Test def test_pl_trailingskip_err() = { runner.runOneTest("pl_trailingskip_err") }
  @Test def test_pl_lengthunits_err() = { runner.runOneTest("pl_lengthunits_err") }
  @Test def test_pl_nest_err() = { runner.runOneTest("pl_nest_err") }
  @Test def test_pl_decimal_err() = { runner.runOneTest("pl_decimal_err") }

  // DAFFODIL-2657
  @Test def test_pl_implicit_1() = { runner.runOneTest("pl_implicit_1") }

  // DAFFODIL-2656
  @Test def test_pl_complexContentLengthBytes_1() = {
    runner.runOneTest("pl_complexContentLengthBytes_1")
  }
  @Test def test_pl_complexValueLengthBytes_1() = {
    runner.runOneTest("pl_complexValueLengthBytes_1")
  }
  @Test def test_pl_complexContentLengthBits_1() = {
    runner.runOneTest("pl_complexContentLengthBits_1")
  }
  @Test def test_pl_complexValueLengthBits_1() = {
    runner.runOneTest("pl_complexValueLengthBits_1")
  }
  @Test def test_pl_simpleContentLengthBytes_1() = {
    runner.runOneTest("pl_simpleContentLengthBytes_1")
  }

  // DAFFODIL-2658
  // @Test def test_pl_simpleValueLengthBytes_1() = { runner.runOneTest("pl_simpleValueLengthBytes_1")}

  @Test def test_pl_simpleContentLengthCharacters_1() = {
    runner.runOneTest("pl_simpleContentLengthCharacters_1")
  }
  @Test def test_pl_complexContentLengthCharacters_1() = {
    runner.runOneTest("pl_complexContentLengthCharacters_1")
  }
  @Test def test_pl_complexContentLengthCharacters_utf8_1() = {
    runner.runOneTest("pl_complexContentLengthCharacters_utf8_1")
  }
  @Test def lengthUnitsBitsForNonNegativeInteger_prefixed(): Unit = {
    runner.runOneTest("lengthUnitsBitsForNonNegativeInteger_prefixed")
  }

  @Test def test_invalidUnsignedLongBitLength(): Unit = {
    runner.runOneTest("invalidUnsignedLongBitLength")
  }
  @Test def test_invalidUnsignedLongByteLength(): Unit = {
    runner.runOneTest("invalidUnsignedLongByteLength")
  }
  @Test def test_invalidUnsignedIntBitLength(): Unit = {
    runner.runOneTest("invalidUnsignedIntBitLength")
  }
  @Test def test_invalidUnsignedShortBitLength(): Unit = {
    runner.runOneTest("invalidUnsignedShortBitLength")
  }
  @Test def test_invalidUnsignedByteBitLength(): Unit = {
    runner.runOneTest("invalidUnsignedByteBitLength")
  }

  @Test def test_invalidLongBitLength(): Unit = {
    runner.runOneTest("invalidLongBitLength")
  }
  @Test def test_invalidIntBitLength(): Unit = {
    runner.runOneTest("invalidIntBitLength")
  }
  @Test def test_invalidShortBitLength(): Unit = {
    runner.runOneTest("invalidShortBitLength")
  }
  @Test def test_invalidByteBitLength(): Unit = {
    runner.runOneTest("invalidByteBitLength")
  }
}
