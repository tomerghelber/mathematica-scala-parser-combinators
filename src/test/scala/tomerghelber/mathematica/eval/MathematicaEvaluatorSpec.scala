package tomerghelber.mathematica.eval

import org.scalacheck.Arbitrary
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tomerghelber.mathematica._
import tomerghelber.mathematica.ast.{NumberNode, SymbolNode}
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
      forAll { (eval: MathematicaEvaluator, expected: NumberNode) =>
        val actual = eval.eval(expected)
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
