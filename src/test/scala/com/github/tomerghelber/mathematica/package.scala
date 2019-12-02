package com.github.tomerghelber

import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.eval.MathematicaEvaluator
import com.github.tomerghelber.mathematica.parser.MathematicaParser
import org.scalacheck.Gen

/**
 * @author user
 * @since 01-Nov-19
 */
package object mathematica {
  // Singletons
  val mathematicaParserGen: Gen[MathematicaParser] = Gen.const(1).map(_ => new MathematicaParser())
  val mathematicaEvaluatorGen: Gen[MathematicaEvaluator] = Gen.const(1).map(_ => new MathematicaEvaluator())

  // Strings
  /** Generates a Unicode character, excluding noncharacters and invalid standalone surrogates: */
  val unicodeChar: Gen[Char] = Gen.frequency(
    Seq(0 -> 55295, 57344 -> 65533).map(x => (1 + x._2 - x._1, Gen.choose(x._1.toChar, x._2.toChar))):_*
  )
  val unicode: Gen[String] = Gen.listOf(unicodeChar).map(_.mkString)
  private val stringWithoutWrappersGen = unicode
    .map(_.replaceAll("\\\\", "\\\\")
      .replaceAll("\"", "\\\""))
  val stringStringGen: Gen[String] = stringWithoutWrappersGen.map(s => "\"" + s + "\"")
  val symbolStringGen: Gen[String] = for (head <- Gen.alphaChar; last <- Gen.alphaNumStr) yield { head + last }
  val integerStringGen: Gen[String] = Gen.chooseNum(Long.MinValue, Long.MaxValue).map(_.toString)
  val rationalStringGen: Gen[String] = for(q <-integerStringGen; p <-integerStringGen) yield { q + "/" + p}
  val floatStringGen: Gen[String] = Gen.chooseNum(Double.MinValue, Double.MaxValue).map(_.toString)
  val scientificNotationGen: Gen[String] = Gen.chooseNum(Double.MinValue, Double.MaxValue).map(BigDecimal(_))
    .map(_.toString)
  val numberStringGen: Gen[String] = Gen.oneOf(integerStringGen, rationalStringGen, floatStringGen,
    scientificNotationGen)

  // Factories
  private def functionNodeFactory(depth: Int=2, maxSpan: Int=3): Gen[FunctionNode] = {
    val argumentsGen = if (depth > 0) {
      Gen.oneOf(terminalNodeGen, functionNodeFactory(depth - 1, maxSpan))
    } else {
      terminalNodeGen
    }
    for {
      name <- symbolNodeGen
      arguments <- Gen.listOfN(maxSpan, argumentsGen)
    } yield FunctionNode(name, arguments)
  }

  // Nodes
  val symbolNodeGen: Gen[SymbolNode] = symbolStringGen.map(SymbolNode)
  val stringNodeGen: Gen[StringNode] = stringWithoutWrappersGen.map(StringNode)
  val numberNodeGen: Gen[NumberNode] = numberStringGen.map(NumberNode)
  val terminalNodeGen: Gen[TerminalNode] = Gen.oneOf(symbolNodeGen, stringNodeGen, numberNodeGen)
  val functionNodeGen: Gen[FunctionNode] = functionNodeFactory()
  val nodeGen: Gen[ASTNode] = Gen.oneOf(terminalNodeGen, Gen.lzy(functionNodeGen))
}
