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
    forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
      val actual = p.parse(first.value + " / " + second.value)
      val expected = DivideNode(
        first,
        second,
      )
      actual shouldBe expected
    }
  }

  test("Simple power") {
    forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
      val actual = p.parse(first.value + " ^ " + second.value)
      val expected = PowerNode(
        first,
        second,
      )
      actual shouldBe expected
    }
  }

  test("Simple subscript") {
    forAll { (p: MathematicaParser, first: SymbolNode, second: SymbolNode) =>
      val actual = p.parse(first.value + " \\_ " + second.value)
      val expected = SubscriptNode(
        first,
        second,
      )
      actual shouldBe expected
    }
  }

  test("Simple factor") {
    forAll { (p: MathematicaParser, first: SymbolNode) =>
      val actual = p.parse(first.value + "!")
      val expected = FactorialNode(
        first,
      )
      actual shouldBe expected
    }
  }

  test("Simple factor2") {
    forAll { (p: MathematicaParser, first: SymbolNode) =>
      val actual = p.parse(first.value + "!!")
      val expected = Factorial2Node(
        first,
      )
      actual shouldBe expected
    }
  }

  test("x \\_ y \\_ z = x \\_ (y \\_ z)") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x \\_ $y \\_ $z")
      val expected = p.parse(f"$x \\_ ($y \\_ $z)")
      actual shouldBe expected
    }
  }

  test("x+y+z = x+(y+z)") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x+$y+$z")
      val expected = p.parse(f"$x+($y+$z)")
      actual shouldBe expected
    }
  }

  test("x*y*z = x*(y*z)") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x*$y*$z")
      val expected = p.parse(f"$x*($y*$z)")
      actual shouldBe expected
    }
  }

  test("x/y/z = (x/y)/z") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x/$y/$z")
      val expected = p.parse(f"($x/$y)/$z")
      actual shouldBe expected
    }
  }

  test("x^y^z = x^(y^z)") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x^$y^$z")
      val expected = p.parse(f"$x^($y^$z)")
      actual shouldBe expected
    }
  }

  test("Preincrement is before Increment") {
    forAll { (p: MathematicaParser, x: SymbolNode) =>
      val actual = p.parse(f"++${x.value}++")
      val expected = PreincrementNode(IncrementNode(x))
      actual shouldBe expected
    }
  }

  test("Plus is before Preincrement") {
    forAll { (p: MathematicaParser, x: SymbolNode, y: SymbolNode) =>
      val actual = p.parse(f"${x.value}+++${y.value}")
      val expected = PlusNode(IncrementNode(x), y)
      actual shouldBe expected
    }
  }

  test("x y z = x*y*z") {
    forAll(mathematicaParserGen, symbolStringGen, symbolStringGen, symbolStringGen) { (p: MathematicaParser, x: String, y: String, z: String) =>
      val actual = p.parse(f"$x $y $z")
      val expected = p.parse(f"$x * $y * $z")
      actual shouldBe expected
    }
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
