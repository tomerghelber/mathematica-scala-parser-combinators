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
class AssociativeSpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {
  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)

  it("Should not work when symbol is not as function name") {
    forAll { (name: SymbolNode, arguments: Seq[ASTNode], other: SymbolNode) =>
      whenever(name != other) {
        val tested = Associative(name)
        val functionNode = FunctionNode(other, arguments)
        tested.apply(functionNode) shouldBe functionNode
      }
    }
  }

  it("Should be possible if get the same symbol") {
    forAll { (name: SymbolNode, multiArguments: Seq[Seq[ASTNode]]) =>
      val tested = Associative(name)
      val functionNode = FunctionNode(name, multiArguments.map(FunctionNode(name, _)))
      val actual = tested.apply(functionNode)
      val expected = FunctionNode(name, multiArguments.flatten)
      actual shouldBe expected
    }
  }
}
