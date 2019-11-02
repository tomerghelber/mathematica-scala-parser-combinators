package tomerghelber.mathematica.eval

import org.scalacheck.Arbitrary
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tomerghelber.mathematica._
import tomerghelber.mathematica.ast.{FunctionNode, NumberNode, SymbolNode}
import tomerghelber.mathematica.parser.MathematicaParser

import scala.collection.mutable

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluatorSpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  implicit def arbMathematicaEvaluator: Arbitrary[MathematicaEvaluator] = Arbitrary(mathematicaEvaluatorGen)
  implicit def arbMathematicaParser: Arbitrary[MathematicaParser] = Arbitrary(mathematicaParserGen)
  implicit def arbNumberNode: Arbitrary[NumberNode] = Arbitrary(numberNodeGen)
  implicit def arbSymbolNode: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)

  describe("Simple") {
    it("number evaluated") {
      forAll { (eval: MathematicaEvaluator, base: NumberNode) =>
        val actual = eval.eval(base)
        val expected = if (base.value.contains("/")) {
          NumberNode(base.value.split("/").map(_.toDouble).reduce(_ / _).toString)
        } else {
          base
        }
        actual shouldBe expected
      }
    }

    it("symbol evaluated") {
      forAll { (symbol: SymbolNode, expected: NumberNode) =>
        val eval = new MathematicaEvaluator(mutable.Map((symbol.value, expected)))
        val actual = eval.eval(symbol)
        actual shouldBe expected
      }
    }

    it("symbol is unknown") {
      forAll { (eval: MathematicaEvaluator, symbol: SymbolNode) =>
        a [NoSuchElementException] should be thrownBy eval.eval(symbol)
      }
    }

    it("plus evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        val actual = eval.eval(FunctionNode(SymbolNode("Plus"), Seq(first, second)))
        val expected = NumberNode((eval.eval(first), eval.eval(second)) match {
          case (NumberNode(a), NumberNode(b)) => (a.toDouble + b.toDouble).toString
          case other => throw new MatchError(other)
        })
        actual shouldBe expected
      }
    }

    it("times evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        val actual = eval.eval(FunctionNode(SymbolNode("Times"), Seq(first, second)))
        val expected = NumberNode((eval.eval(first), eval.eval(second)) match {
          case (NumberNode(a), NumberNode(b)) => (a.toDouble * b.toDouble).toString
          case other => throw new MatchError(other)
        })
        actual shouldBe expected
      }
    }

    it("divide evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        whenever(second.value.toDouble != 0) {
          val actual = eval.eval(FunctionNode(SymbolNode("Divide"), Seq(first, second)))
          val expected = NumberNode((eval.eval(first), eval.eval(second)) match {
            case (NumberNode(a), NumberNode(b)) => (a.toDouble / b.toDouble).toString
            case other => throw new MatchError(other)
          })
          actual shouldBe expected
        }
      }
    }

    it("wrong number of parameters evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode) =>
        a [MatchError] should be thrownBy eval.eval(FunctionNode(SymbolNode("Divide"), Seq(first, first, first)))
      }
    }
  }

  describe("Examples from WolfRam site") {
    ignore("https://www.wolfram.com/language/gallery/implement-hello-world-in-the-cloud/") {
      forAll { (eval: MathematicaEvaluator, p: MathematicaParser) =>

        val out1 = eval.eval(p.parse("\"Hello, World\""))
        val out2 = eval.eval(p.parse("Do[Print[\"Hello, World\"], {5}]"))
        val out3 = eval.eval(p.parse("CloudObject[\"Hello, World\"]"))
        val out4 = eval.eval(p.parse(
          "CloudDeploy[\n" +
            " ExportForm[Style[Framed[\"Hello, World\", ImageMargins -> 60],\n" +
            "   80, Orange, FontFamily -> \"Verdana\"], \"GIF\"], \n" +
            " Permissions -> \"Public\"]"
        ))
      }
    }

    ignore("https://www.wolfram.com/language/gallery/make-a-hipstamatic-filter/") {
      forAll { (eval: MathematicaEvaluator, p: MathematicaParser) =>
        val out1 = eval.eval(p.parse(
          "CloudDeploy[\n" +
            " FormFunction[FormObject[{\"ImageURL\" -> \"String\"}], \n" +
            "  ImageEffect[\n" +
            "    ColorConvert[\n" +
            "     ImageMultiply[\n" +
            "      ColorConvert[\n" +
            "       ImageAdd[ImageAdjust[Import[#ImageURL], .2], \n" +
            "        RGBColor[.25, .25, -.1]], \"HSB\"], Hue[1, .7, 1]], \n" +
            "     \"RGB\"], {\"PoissonNoise\", .5}] &, \"JPEG\"], \n" +
            " Permissions -> \"Public\"]"
        ))
      }
    }
  }
}
