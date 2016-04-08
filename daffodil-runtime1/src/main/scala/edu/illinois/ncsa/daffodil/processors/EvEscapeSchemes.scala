package edu.illinois.ncsa.daffodil.processors

import edu.illinois.ncsa.daffodil.dsom._
import edu.illinois.ncsa.daffodil.util.MaybeChar
import edu.illinois.ncsa.daffodil.schema.annotation.props.gen.GenerateEscape
import edu.illinois.ncsa.daffodil.util.Maybe


class EscapeCharEv(expr: CompiledExpression[String], rd: RuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeCharacterCooker,
    rd) {
  override lazy val runtimeDependencies = Nil
}

class EscapeEscapeCharEv(expr: CompiledExpression[String], rd: RuntimeData)
  extends EvaluatableConvertedExpression[String, String](
    expr,
    EscapeEscapeCharacterCooker,
    rd) {
  override lazy val runtimeDependencies = Nil
}



trait EscapeSchemeCommonEv {
  def optEscapeEscapeChar: Maybe[EscapeEscapeCharEv]

  def evalAndConvertEEC(state: ParseOrUnparseState): MaybeChar = {
    if (optEscapeEscapeChar.isDefined) {
      val escEscChar = optEscapeEscapeChar.get.evaluate(state)
      MaybeChar(escEscChar.charAt(0))
    } else {
      MaybeChar.Nope
    }
  }
}

abstract class EscapeSchemeParseEv(rd: RuntimeData)
  extends Evaluatable[EscapeSchemeParserHelper](rd)
  with EscapeSchemeCommonEv

abstract class EscapeSchemeUnparseEv(rd: RuntimeData)
  extends Evaluatable[EscapeSchemeUnparserHelper](rd)
  with EscapeSchemeCommonEv {

  def extraEscapedChars: Maybe[String]

  val extraEscapedCharsCooked = {
    if (extraEscapedChars.isDefined) {
      ExtraEscapedCharactersCooker.convertConstant(extraEscapedChars.get, rd, forUnparse = true).map { _.charAt(0) }
    } else {
      Seq()
    }
  }
}


class EscapeSchemeCharParseEv(escapeChar: EscapeCharEv,
                              override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
                              rd: RuntimeData)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = List(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharParserHelper(escChar, optEscEscChar)
  }
}

class EscapeSchemeCharUnparseEv(escapeChar: EscapeCharEv,
                                override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
                                override val extraEscapedChars: Maybe[String],
                                rd: RuntimeData)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = List(escapeChar) ++ optEscapeEscapeChar.toList

  def compute(state: ParseOrUnparseState) = {
    val escChar = escapeChar.evaluate(state).charAt(0)
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeCharUnparserHelper(escChar, optEscEscChar, extraEscapedCharsCooked)
  }
}

class EscapeSchemeBlockParseEv(blockStart: String,
                               blockEnd: String,
                               override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
                               rd: RuntimeData)
  extends EscapeSchemeParseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = false)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = false)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockParserHelper(optEscEscChar, bs, be)
  }
}

class EscapeSchemeBlockUnparseEv(blockStart: String,
                                 blockEnd: String,
                                 override val optEscapeEscapeChar: Maybe[EscapeEscapeCharEv],
                                 override val extraEscapedChars: Maybe[String],
                                 generateEscapeBlock: GenerateEscape,
                                 rd: RuntimeData)
  extends EscapeSchemeUnparseEv(rd) {

  override val runtimeDependencies = optEscapeEscapeChar.toList

  val bs = EscapeBlockStartCooker.convertConstant(blockStart, rd, forUnparse = true)
  val be = EscapeBlockEndCooker.convertConstant(blockEnd, rd, forUnparse = true)

  def compute(state: ParseOrUnparseState) = {
    val optEscEscChar = evalAndConvertEEC(state)
    new EscapeSchemeBlockUnparserHelper(optEscEscChar, bs, be, extraEscapedCharsCooked, generateEscapeBlock)
  }
}
