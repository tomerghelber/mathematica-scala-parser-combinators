package tomerghelber.mathematica.parser

import org.scalatest.{FunSuite, Matchers}
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import tomerghelber.mathematica.ast._

class MathematicaParserSpec extends FunSuite with Matchers with ScalaCheckPropertyChecks {

  val symbolStringGen = Gen.alphaStr
  val symbolNodeGen = symbolStringGen.map(SymbolNode)
  val mathematicaParserGen = Gen.choose(1, 1).map(_ => new MathematicaParser())

  test("Simple symbol") {
    forAll (mathematicaParserGen, symbolStringGen) { (p: MathematicaParser, symbolString: String) =>
      val actual = p.parse(symbolString)
      val expected = SymbolNode(symbolString)
      actual shouldBe expected
    }
  }

  test("Simple integer") {
    val p = new MathematicaParser()
    val actual = p.parse("-4242424242424242")
    val expected = IntegerNode(BigInt("-4242424242424242"))
    actual shouldBe expected
  }

  test("Simple float") {
    val p = new MathematicaParser()
    val actual = p.parse("-4242424242424242.125")
    val expected = FloatNode(-4242424242424242.125)
    actual shouldBe expected
  }

  test("Simple scientific notation") {
    val p = new MathematicaParser()
    val actual = p.parse("-4242424242424242.125E-1")
    val expected = FloatNode(-424242424242424.2125)
    actual shouldBe expected
  }

  test("Simple plus") {
    val p = new MathematicaParser()
    val actual = p.parse("a + b")
    val expected = PlusNode(
      SymbolNode("a"),
      SymbolNode("b"),
    )
    actual shouldBe expected
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
