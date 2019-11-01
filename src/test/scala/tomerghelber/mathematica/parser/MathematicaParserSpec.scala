package tomerghelber.mathematica.parser

import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tomerghelber.mathematica._
import tomerghelber.mathematica.ast._

class MathematicaParserSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks {

  implicit def arbSymbolNode: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  implicit def arbMathematicaParser: Arbitrary[MathematicaParser] = Arbitrary(mathematicaParserGen)

  test("Simple symbol") {
    forAll { (p: MathematicaParser, expected: SymbolNode) =>
      val actual = p.parse(expected.value)
      actual shouldBe expected
    }
  }

  test("Simple integer") {
    forAll (mathematicaParserGen, integerStringGen) { (p: MathematicaParser, integerString: String) =>
      val actual = p.parse(integerString)
      val expected = NumberNode(integerString.toDouble)
      actual shouldBe expected
    }
  }

  test("Simple float") {
    forAll (mathematicaParserGen, floatStringGen) { (p: MathematicaParser, floatString: String) =>
      val actual = p.parse(floatString)
      val expected = NumberNode(floatString.toDouble)
      actual shouldBe expected
    }
  }

  test("Simple scientific notation") {
    forAll (mathematicaParserGen, scientificNotationGen) { (p: MathematicaParser, scientificNotation: String) =>
      val actual = p.parse(scientificNotation)
      val expected = NumberNode(scientificNotation.toDouble)
      actual shouldBe expected
    }
  }

  test("Simple numbers") {
    forAll (mathematicaParserGen, numberStringGen) { (p: MathematicaParser, numberString: String) =>
      val actual = p.parse(numberString)
      val expected = NumberNode(numberString.toDouble)
      actual shouldBe expected
    }
  }

  test("Simple plus") {
    forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
      val actual = p.parse(first.value + " + " + second.value)
      val expected = PlusNode(
        first,
        second,
      )
      actual shouldBe expected
    }
  }

  test("Simple divide") {
    val p = new MathematicaParser()
    val actual = p.parse("a / b")
    val expected = DivideNode(
      SymbolNode("a"),
      SymbolNode("b"),
    )
    actual shouldBe expected
  }

  test("Simple power") {
    val p = new MathematicaParser()
    val actual = p.parse("a ^ b")
    val expected = PowerNode(
      SymbolNode("a"),
      SymbolNode("b"),
    )
    actual shouldBe expected
  }

  test("Simple factor") {
    val p = new MathematicaParser()
    val actual = p.parse("a!")
    val expected = FactorialNode(
      SymbolNode("a"),
    )
    actual shouldBe expected
  }

  test("Simple factor2") {
    val p = new MathematicaParser()
    val actual = p.parse("a!!")
    val expected = Factorial2Node(
      SymbolNode("a"),
    )
    actual shouldBe expected
  }

  test("x+y+z = x+(y+z)") {
    val p = new MathematicaParser()
    val actual = p.parse("x+y+z")
    val expected = p.parse("x+(y+z)")
    actual shouldBe expected
  }

  test("x*y*z = x*(y*z)") {
    val p = new MathematicaParser()
    val actual = p.parse("x*y*z")
    val expected = p.parse("x*(y*z)")
    actual shouldBe expected
  }

  test("x/y/z = (x/y)/z") {
    val p = new MathematicaParser()
    val actual = p.parse("x/y/z")
    val expected = p.parse("(x/y)/z")
    actual shouldBe expected
  }

  test("x^y^z = x^(y^z)") {
    val p = new MathematicaParser()
    val actual = p.parse("x^y^z")
    val expected = p.parse("x^(y^z)")
    actual shouldBe expected
  }

  test("Preincrement is before Increment") {
    val p = new MathematicaParser()
    val actual = p.parse("++x++")
    val expected = PreincrementNode(IncrementNode(SymbolNode("x")))
    actual shouldBe expected
  }

  test("Plus is before Preincrement") {
    val p = new MathematicaParser()
    val actual = p.parse("x+++y")
    val expected = PlusNode(IncrementNode(SymbolNode("x")), SymbolNode("y"))
    actual shouldBe expected
  }

  test("x y z = x*y*z") {
    val p = new MathematicaParser()
    val actual = p.parse("x y z")
    val expected = p.parse("x*y*z")
    actual shouldBe expected
  }

  test("2x = 2*x") {
    val p = new MathematicaParser()
    val actual = p.parse("2x")
    val expected = p.parse("2*x")
    actual shouldBe expected
  }

  test("2(x+1) = 2*(x+1)") {
    val p = new MathematicaParser()
    val actual = p.parse("2(x+1)")
    val expected = p.parse("2*(x+1)")
    actual shouldBe expected
  }

  test("c(x+1) = c*(x+1)") {
    val p = new MathematicaParser()
    val actual = p.parse("c(x+1)")
    val expected = p.parse("c*(x+1)")
    actual shouldBe expected
  }

  test("(x+1)(y+2) = (x+1)*(y+2)") {
    val p = new MathematicaParser()
    val actual = p.parse("(x+1)(y+2)")
    val expected = p.parse("(x+1)*(y+2)")
    actual shouldBe expected
  }

  test("x! y = x!*y") {
    val p = new MathematicaParser()
    val actual = p.parse("x! y")
    val expected = p.parse("x!*y")
    actual shouldBe expected
  }

  test("x!y = x!*y") {
    val p = new MathematicaParser()
    val actual = p.parse("x!y")
    val expected = p.parse("x!*y")
    actual shouldBe expected
  }

  test("x^2y = x^2 y = (x^2) y") {
    val p = new MathematicaParser()
    val actual1 = p.parse("x^2y")
    val actual2 = p.parse("x^2 y")
    val expected = p.parse("(x^2) y")
    actual1 shouldBe expected
    actual2 shouldBe expected
  }

  test("x/2y = x/2 y = (x/2) y") {
    val p = new MathematicaParser()
    val actual1 = p.parse("x/2y")
    val actual2 = p.parse("x/2 y")
    val expected = p.parse("(x/2) y")
    actual1 shouldBe expected
    actual2 shouldBe expected
  }
}
