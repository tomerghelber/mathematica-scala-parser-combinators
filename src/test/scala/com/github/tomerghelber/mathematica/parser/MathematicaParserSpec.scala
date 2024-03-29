package com.github.tomerghelber.mathematica
package parser

import com.github.tomerghelber.mathematica.ast.SymbolNode
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AsyncFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.github.tomerghelber.mathematica.ast._

class MathematicaParserSpec extends AsyncFunSpec with Matchers with ScalaCheckPropertyChecks {

  implicit def arbSymbolNode: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  implicit def arbMathematicaParser: Arbitrary[MathematicaParser] = Arbitrary(mathematicaParserGen)

  describe("Existence tests") {
    it("Simple symbol") {
      forAll { (p: MathematicaParser, expected: SymbolNode) =>
        val actual = p.parse(expected.value)
        actual shouldBe expected
      }
    }

    it("Simple string") {
      forAll(mathematicaParserGen, stringStringGen) { (p: MathematicaParser, stringString: String) =>
        val actual = p.parse(stringString)
        val expected = StringNode(stringString.substring(1, stringString.length - 1))
        actual shouldBe expected
      }
    }

    it("Simple integer") {
      forAll(mathematicaParserGen, integerStringGen) { (p: MathematicaParser, integerString: String) =>
        val actual = p.parse(integerString)
        val expected = NumberNode(integerString)
        actual shouldBe expected
      }
    }

    it("Simple float") {
      forAll(mathematicaParserGen, floatStringGen) { (p: MathematicaParser, floatString: String) =>
        val actual = p.parse(floatString)
        val expected = NumberNode(floatString)
        actual shouldBe expected
      }
    }

    it("Simple scientific notation") {
      forAll(mathematicaParserGen, scientificNotationGen) { (p: MathematicaParser, scientificNotation: String) =>
        val actual = p.parse(scientificNotation)
        val expected = NumberNode(scientificNotation)
        actual shouldBe expected
      }
    }

    it("Simple numbers") {
      forAll(mathematicaParserGen, numberStringGen) { (p: MathematicaParser, numberString: String) =>
        val actual = p.parse(numberString)
        val expected = NumberNode(numberString)
        actual shouldBe expected
      }
    }

    it("Simple plus") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " + " + second.value)
        val expected = PlusNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple divide") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " / " + second.value)
        val expected = DivideNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple power") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " ^ " + second.value)
        val expected = PowerNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple overscript") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " \\& " + second.value)
        val expected = OverscriptNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple underscript") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " \\+ " + second.value)
        val expected = UnderscriptNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple subscript") {
      forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
        val actual = p.parse(first.value + " \\_ " + second.value)
        val expected = SubscriptNode(
          first,
          second,
        )
        actual shouldBe expected
      }
    }

    it("Simple factor") {
      forAll { (p: MathematicaParser, first: SymbolNode) =>
        val actual = p.parse(first.value + "!")
        val expected = FactorialNode(
          first,
        )
        actual shouldBe expected
      }
    }

    it("Simple factor2") {
      forAll { (p: MathematicaParser, first: SymbolNode) =>
        val actual = p.parse(first.value + "!!")
        val expected = Factorial2Node(
          first,
        )
        actual shouldBe expected
      }
    }
  }

  describe("check order in same operators") {
    it("x \\+ y \\+ z = x \\+ (y \\+ z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x \\+ $y \\+ $z")
        val expected = p.parse(f"$x \\+ ($y \\+ $z)")
        actual shouldBe expected
      }
    }

    it("x \\& y \\& z = x \\& (y \\& z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x \\& $y \\& $z")
        val expected = p.parse(f"$x \\& ($y \\& $z)")
        actual shouldBe expected
      }
    }

    it("x \\_ y \\_ z = x \\_ (y \\_ z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x \\_ $y \\_ $z")
        val expected = p.parse(f"$x \\_ ($y \\_ $z)")
        actual shouldBe expected
      }
    }

    it("x+y+z = x+(y+z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x+$y+$z")
        val expected = p.parse(f"$x+($y+$z)")
        actual shouldBe expected
      }
    }

    it("x*y*z = x*(y*z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x*$y*$z")
        val expected = p.parse(f"$x*($y*$z)")
        actual shouldBe expected
      }
    }

    it("x/y/z = (x/y)/z") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x/$y/$z")
        val expected = p.parse(f"($x/$y)/$z")
        actual shouldBe expected
      }
    }

    it("x^y^z = x^(y^z)") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x^$y^$z")
        val expected = p.parse(f"$x^($y^$z)")
        actual shouldBe expected
      }
    }

    it("x y z = x*y*z") {
      forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
        val actual = p.parse(f"$x $y $z")
        val expected = p.parse(f"$x * $y * $z")
        actual shouldBe expected
      }
    }

    it("2x = 2*x") {
      val p = new MathematicaParser()
      val actual = p.parse("2x")
      val expected = p.parse("2*x")
      actual shouldBe expected
    }
  }
  describe("check order between diffrenet operators") {
    it("Preincrement is before Increment") {
      forAll { (p: MathematicaParser, x: SymbolNode) =>
        val actual = p.parse(f"++${x.value}++")
        val expected = PreincrementNode(IncrementNode(x))
        actual shouldBe expected
      }
    }

    it("Plus is before Preincrement") {
      forAll { (p: MathematicaParser, x: SymbolNode, y: SymbolNode) =>
        val actual = p.parse(f"${x.value}+++${y.value}")
        val expected = PlusNode(IncrementNode(x), y)
        actual shouldBe expected
      }
    }

    it("2(x+1) = 2*(x+1)") {
      val p = new MathematicaParser()
      val actual = p.parse("2(x+1)")
      val expected = p.parse("2*(x+1)")
      actual shouldBe expected
    }

    it("c(x+1) = c*(x+1)") {
      val p = new MathematicaParser()
      val actual = p.parse("c(x+1)")
      val expected = p.parse("c*(x+1)")
      actual shouldBe expected
    }

    it("(x+1)(y+2) = (x+1)*(y+2)") {
      val p = new MathematicaParser()
      val actual = p.parse("(x+1)(y+2)")
      val expected = p.parse("(x+1)*(y+2)")
      actual shouldBe expected
    }

    it("x! y = x!*y") {
      val p = new MathematicaParser()
      val actual = p.parse("x! y")
      val expected = p.parse("x!*y")
      actual shouldBe expected
    }

    it("x!y = x!*y") {
      val p = new MathematicaParser()
      val actual = p.parse("x!y")
      val expected = p.parse("x!*y")
      actual shouldBe expected
    }

    it("x^2y = x^2 y = (x^2) y") {
      val p = new MathematicaParser()
      val actual1 = p.parse("x^2y")
      val actual2 = p.parse("x^2 y")
      val expected = p.parse("(x^2) y")
      actual1 shouldBe expected
      actual2 shouldBe expected
    }

    it("x/2y = x/2 y = (x/2) y") {
      val p = new MathematicaParser()
      val actual1 = p.parse("x/2y")
      val actual2 = p.parse("x/2 y")
      val expected = p.parse("(x/2) y")
      actual1 shouldBe expected
      actual2 shouldBe expected
    }
  }


  describe("Examples from WolfRam site") {
    describe("https://www.wolfram.com/language/gallery/implement-hello-world-in-the-cloud/") {
      it("[1]") {
        forAll { p: MathematicaParser =>

          val out1 = p.parse("\"Hello, World\"")
          out1 shouldBe StringNode("Hello, World")
        }
      }
      ignore("[2]") {
        forAll { p: MathematicaParser =>
          val out2 = p.parse("Do[Print[\"Hello, World\"], {5}]")
          out2 shouldBe FunctionNode(SymbolNode("Do"), Seq(
            FunctionNode(SymbolNode("Print"), Seq(
              StringNode("Hello, World")
            )),
            ListNode(Seq(
              NumberNode("5")
            ))
          ))

        }
      }
      it("[3]") {
        forAll { p: MathematicaParser =>
          val out3 = p.parse("CloudObject[\"Hello, World\"]")
          out3 shouldBe FunctionNode(SymbolNode("CloudObject"), Seq(
            StringNode("Hello, World"),
          ))
        }
      }
      ignore("[4]") {
        forAll { p: MathematicaParser =>
          val out4 = p.parse(
            "CloudDeploy[\n" +
              " ExportForm[Style[Framed[\"Hello, World\", ImageMargins -> 60],\n" +
              "   80, Orange, FontFamily -> \"Verdana\"], \"GIF\"], \n" +
              " Permissions -> \"Public\"]"
          )
          out4 shouldBe FunctionNode(SymbolNode("CloudDeploy"), Seq(
            FunctionNode(SymbolNode("ExportForm"), Seq(
              FunctionNode(SymbolNode("Style"), Seq(
                FunctionNode(SymbolNode("Framed"), Seq(
                  StringNode("Hello, World"),
                  StringNode("ImageMargins -> 60"),
                )),
                StringNode("80"),
                StringNode("Orange"),
                StringNode("FontFamily -> \"Verdana\""),
              )),
              StringNode("GIF"),
            )),
            StringNode("Permissions -> \"Public\""),
          ))
        }
      }
    }

    ignore("https://www.wolfram.com/language/gallery/make-a-hipstamatic-filter/") {
      forAll { p: MathematicaParser =>
        val out1 = p.parse(
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
        )
        out1 should not be null
      }
    }
  }
}
