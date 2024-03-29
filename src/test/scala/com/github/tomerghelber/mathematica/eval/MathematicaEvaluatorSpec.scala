package com.github.tomerghelber.mathematica
package eval

import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AsyncFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.github.tomerghelber.mathematica.ast.{DivideNode, FunctionNode, NumberNode, PlusNode, SymbolNode, TimesNode}
import com.github.tomerghelber.mathematica.parser.MathematicaParser

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluatorSpec extends AsyncFunSpec with Matchers with ScalaCheckPropertyChecks {

  implicit def arbMathematicaEvaluator: Arbitrary[MathematicaEvaluator] = Arbitrary(mathematicaEvaluatorGen)
  implicit def arbMathematicaParser: Arbitrary[MathematicaParser] = Arbitrary(mathematicaParserGen)
  implicit def arbNumberNode: Arbitrary[NumberNode] = Arbitrary(numberNodeGen)
  implicit def arbSymbolNode: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)

  describe("Simple") {
    it("number evaluated") {
      forAll { (eval: MathematicaEvaluator, base: NumberNode) =>
        val actual = eval.eval(base)
        actual shouldBe a[NumberNode]
        withClue("Result number " + actual.asInstanceOf[NumberNode].value) {
          noException should be thrownBy BigDecimal(actual.asInstanceOf[NumberNode].value)
        }
      }
    }

    it("symbol evaluated") {
      forAll { (symbol: SymbolNode, expected: NumberNode) =>
        val eval = new MathematicaEvaluator(Map((symbol.value, expected)))
        val actual = eval.eval(symbol)
        actual shouldBe expected
      }
    }

    it("symbol is unknown") {
      forAll { (eval: MathematicaEvaluator, symbol: SymbolNode) =>
        a[NoSuchElementException] should be thrownBy eval.eval(symbol)
      }
    }

    it("plus evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        val actualNode = eval.eval(PlusNode(first, second))
        val expected = (eval.eval(first), eval.eval(second)) match {
          case (NumberNode(a), NumberNode(b)) => BigDecimal(a) + BigDecimal(b)
          case other: Any => throw new MatchError(other)
        }
        actualNode shouldBe a[NumberNode]
        val actual = BigDecimal(actualNode.asInstanceOf[NumberNode].value)
        actual shouldBe expected +- 0.000001
      }
    }

    it("times evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        val actual = eval.eval(TimesNode(first, second))
        val expected = NumberNode((eval.eval(first), eval.eval(second)) match {
          case (NumberNode(a), NumberNode(b)) => (BigDecimal(a) * BigDecimal(b)).toString
          case other: Any => throw new MatchError(other)
        })
        actual shouldBe expected
      }
    }

    it("divide not by zero evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode, second: NumberNode) =>
        (eval.eval(first), eval.eval(second)) match {
          case (NumberNode(a), NumberNode(b)) =>
            val lower = BigDecimal(b)
            whenever(lower != 0) {
              val actual = eval.eval(DivideNode(first, second))
              val expected = NumberNode((BigDecimal(a) / lower).toString)
              actual shouldBe expected
            }
          case other: Any => throw new MatchError(other)
        }
      }
    }

    it("divide by zero not evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode) =>
        val second = NumberNode(BigDecimal(0).toString)
        an[ArithmeticException] should be thrownBy  eval.eval(DivideNode(first, second))
      }
    }

    it("wrong number of parameters evaluated") {
      forAll { (eval: MathematicaEvaluator, first: NumberNode) =>
        a[MatchError] should be thrownBy eval.eval(FunctionNode(DivideNode.symbol, Seq(first, first, first)))
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
        out1 should not be null
        out2 should not be null
        out3 should not be null
        out4 should not be null
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
        out1 should not be null
      }
    }
  }
}
