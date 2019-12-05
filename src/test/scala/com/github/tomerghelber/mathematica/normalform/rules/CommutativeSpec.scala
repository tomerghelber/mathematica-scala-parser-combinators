package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.normalform.NormalForm.ASTNodeOrdering
import com.github.tomerghelber.mathematica.{nodeGen, symbolNodeGen}
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * @author user
 * @since 18-Nov-19
 */
class CommutativeSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {
  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)

  it("Should not work when symbol is not as function name") {
    forAll(sizeRange(10), maxDiscardedFactor(100.0)) { (name: SymbolNode, arguments: Seq[ASTNode]) =>
      val other = SymbolNode(name.value + "other")
      val tested = Commutative(name)
      val functionNode = FunctionNode(other, arguments)
      tested.apply(functionNode) shouldBe functionNode
    }
  }

  it("Should be possible if get the same symbol") {
    forAll(sizeRange(10), maxDiscardedFactor(100.0)) { (name: SymbolNode, arguments: Seq[ASTNode]) =>
      val tested = Commutative(name)
      val functionNode = FunctionNode(name, arguments)
      val actual = tested.apply(functionNode)
      val expected = FunctionNode(name, arguments.sorted)
      actual shouldBe expected
    }
  }
}
