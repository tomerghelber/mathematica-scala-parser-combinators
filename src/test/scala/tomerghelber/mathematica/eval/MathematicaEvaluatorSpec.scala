package tomerghelber.mathematica.eval

import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tomerghelber.mathematica._
import tomerghelber.mathematica.ast.{NumberNode, SymbolNode}

import scala.collection.mutable

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluatorSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks {

  implicit def arbMathematicaEvaluator: Arbitrary[MathematicaEvaluator] = Arbitrary(mathematicaEvaluatorGen)
  implicit def arbNumberNode: Arbitrary[NumberNode] = Arbitrary(numberNodeGen)
  implicit def arbSymbolNode: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)

  test("number evaluated") {
    forAll { (eval: MathematicaEvaluator, expected: NumberNode) =>
      val actual = eval.eval(expected)
      actual shouldBe expected
    }
  }

  test("symbol evaluated") {
    forAll {(symbol: SymbolNode, expected: NumberNode) =>
      val eval = new MathematicaEvaluator(mutable.Map((symbol.value, expected)))
      val actual = eval.eval(symbol)
      actual shouldBe expected
    }
  }
}
