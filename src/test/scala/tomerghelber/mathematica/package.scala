package tomerghelber

import org.scalacheck.Gen
import tomerghelber.mathematica.ast.{NumberNode, StringNode, SymbolNode}
import tomerghelber.mathematica.eval.MathematicaEvaluator
import tomerghelber.mathematica.parser.MathematicaParser

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
  private val stringStringWithoutWrappersGen = unicode
    .map(_.replaceAll("\\\\", "\\\\")
      .replaceAll("\"", "\\\""))
  val stringStringGen: Gen[String] = stringStringWithoutWrappersGen.map(s => "\"" + s + "\"")
  val symbolStringGen: Gen[String] = for (head <- Gen.alphaChar; last <- Gen.alphaNumStr) yield { head + last }
  private val posIntegerGen = Gen.numStr.withFilter(_.nonEmpty)
  val integerStringGen: Gen[String] = for (signOpt <- Gen.option("-"); posInteger <-posIntegerGen) yield {
    signOpt match {
      case None => posInteger
      case Some(sign) => sign + posInteger
    }
  }
  val rationalStringGen: Gen[String] = for(q <-integerStringGen; p <-integerStringGen) yield { q + "/" + p}
  val floatStringGen: Gen[String] = for (integer <- integerStringGen; posInteger <- posIntegerGen) yield {
    integer + "." + posInteger
  }
  val scientificNotationGen: Gen[String] = for (base <- floatStringGen; expo <- integerStringGen) yield {
    base + "E" + expo
  }
  val numberStringGen: Gen[String] = Gen.oneOf(integerStringGen, rationalStringGen, floatStringGen,
    scientificNotationGen)

  // Nodes
  val symbolNodeGen: Gen[SymbolNode] = symbolStringGen.map(SymbolNode)
  val stringNodeGen: Gen[StringNode] = stringStringWithoutWrappersGen.map(StringNode)
  val numberNodeGen: Gen[NumberNode] = numberStringGen.map(NumberNode)
}
