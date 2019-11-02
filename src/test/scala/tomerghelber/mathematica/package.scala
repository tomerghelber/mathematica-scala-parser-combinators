package tomerghelber

import org.scalacheck.Gen
import tomerghelber.mathematica.ast.{NumberNode, SymbolNode}
import tomerghelber.mathematica.eval.MathematicaEvaluator
import tomerghelber.mathematica.parser.MathematicaParser

/**
 * @author user
 * @since 01-Nov-19
 */
package object mathematica {
  // Singletons
  val mathematicaParserGen = Gen.choose(1, 1).map(_ => new MathematicaParser())
  val mathematicaEvaluatorGen = Gen.choose(1, 1).map(_ => new MathematicaEvaluator())

  // Strings
  val stringStringGen = Gen.alphaStr.map(s => "\"" + s + "\"")
  val symbolStringGen = Gen.alphaStr.filter(_.nonEmpty)
  private val posIntegerGen = Gen.numStr.withFilter(_.nonEmpty)
  val integerStringGen = for (signOpt <- Gen.option("-"); posInteger <-posIntegerGen) yield {
    signOpt match {
      case None => posInteger
      case Some(sign) => sign + posInteger
    }
  }
  val floatStringGen = for (integer <- integerStringGen; posInteger <- posIntegerGen) yield {
    integer + "." + posInteger
  }
  val scientificNotationGen = for (base <- floatStringGen; expo <- integerStringGen) yield {
    base + "E" + expo
  }
  val numberStringGen = Gen.oneOf(integerStringGen, floatStringGen, scientificNotationGen)

  // Nodes
  val symbolNodeGen = symbolStringGen.map(SymbolNode)
  val numberNodeGen = numberStringGen.map(_.toDouble).map(NumberNode)
}
