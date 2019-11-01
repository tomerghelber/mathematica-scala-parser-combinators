package tomerghelber

import org.scalacheck.Gen
import tomerghelber.mathematica.ast.{NumberNode, SymbolNode}
import tomerghelber.mathematica.parser.MathematicaParser

/**
 * @author user
 * @since 01-Nov-19
 */
package object mathematica {
  // Singletons
  val mathematicaParserGen = Gen.choose(1, 1).map(_ => new MathematicaParser())

  // Strings
  val symbolStringGen = Gen.alphaStr.withFilter(_.nonEmpty)

  // Nodes
  val symbolNodeGen = symbolStringGen.map(SymbolNode)
  val numberNodeGen = Gen.choose(Double.MinValue, Double.MaxValue).map(NumberNode)
}
