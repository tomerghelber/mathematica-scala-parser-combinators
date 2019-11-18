package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast.{NumberNode, StringNode, SymbolNode}
import com.github.tomerghelber.mathematica.{numberNodeGen, symbolNodeGen, stringNodeGen}
import org.scalacheck.Arbitrary
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TerminalNodeOrderingSpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val numberNodeArbitrary: Arbitrary[NumberNode] = Arbitrary(numberNodeGen)
  private implicit val stringNodeArbitrary: Arbitrary[StringNode] = Arbitrary(stringNodeGen)

  it("Symbols should be ordered by their value") {
    forAll { (s1: SymbolNode, s2: SymbolNode) =>
      val expected = TerminalNodeOrdering.compare(s1, s2)
      expected shouldEqual Ordering.String.compare(s1.value, s2.value)
      TerminalNodeOrdering.compare(s2, s1) shouldEqual -expected
    }
  }

  it("Numbers should be ordered by their value") {
    forAll { (s1: NumberNode, s2: NumberNode) =>
      val expected = TerminalNodeOrdering.compare(s1, s2)
      expected shouldEqual Ordering.String.compare(s1.value, s2.value)
      TerminalNodeOrdering.compare(s2, s1) shouldEqual -expected
    }
  }

  it("String should be ordered by their value") {
    forAll { (s1: StringNode, s2: StringNode) =>
      val expected = TerminalNodeOrdering.compare(s1, s2)
      expected shouldEqual Ordering.String.compare(s1.value, s2.value)
      TerminalNodeOrdering.compare(s2, s1) shouldEqual -expected
    }
  }

  it("String should be ordered after numbers") {
    forAll { (s1: NumberNode, s2: StringNode) =>
      TerminalNodeOrdering.compare(s1, s2) shouldEqual -1
      TerminalNodeOrdering.compare(s2, s1) shouldEqual 1
    }
  }

  it("Symbol should be ordered after strings") {
    forAll { (s1: StringNode, s2: SymbolNode) =>
      TerminalNodeOrdering.compare(s1, s2) shouldEqual -1
      TerminalNodeOrdering.compare(s2, s1) shouldEqual 1
    }
  }

  it("Symbol should be ordered after numbers") {
    forAll { (s1: NumberNode, s2: SymbolNode) =>
      TerminalNodeOrdering.compare(s1, s2) shouldEqual -1
      TerminalNodeOrdering.compare(s2, s1) shouldEqual 1
    }
  }
}
