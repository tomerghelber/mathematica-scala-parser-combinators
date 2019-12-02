package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.{nodeGen, symbolNodeGen}
import org.scalacheck.Arbitrary
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * @author user
 * @since 18-Nov-19
 */
class DistributiveSpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {
  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)

  it("Cannot be created with lower and upper the same") {
    forAll { name: SymbolNode =>
      an[IllegalArgumentException] should be thrownBy Distributive(name, name)
    }
  }

  it("Should not work when symbol is not as lower") {
    forAll { (upper: SymbolNode, arguments: Seq[ASTNode]) =>
      val lower = SymbolNode(upper.value + "lower")
      val tested = Distributive(upper, lower)
      val functionNode = FunctionNode(upper, arguments)
      tested.apply(functionNode) shouldBe functionNode
    }
  }

  it("Should be possible if get the lower symbol") {
    forAll {
      (upper: SymbolNode, multiArguments: Seq[Seq[ASTNode]]) =>
      val lower = SymbolNode(upper.value + "lower")
      val tested = Distributive(upper, lower)
      val functionNode = FunctionNode(lower, multiArguments.map(FunctionNode(upper, _)))
      val actual = tested.apply(functionNode)
      val arguments = Distributive.permutations(multiArguments).map(FunctionNode(lower, _))
      val expected = FunctionNode(upper, arguments)
      actual shouldBe expected
    }
  }
}
