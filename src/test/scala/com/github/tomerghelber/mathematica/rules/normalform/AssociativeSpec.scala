package com.github.tomerghelber.mathematica.rules.normalform

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.{nodeGen, symbolNodeGen}
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AsyncFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * @author user
 * @since 18-Nov-19
 */
class AssociativeSpec extends AsyncFunSpec with Matchers with ScalaCheckPropertyChecks {
  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)

  it("Should not work when symbol is not as function name") {
    forAll(sizeRange(10), maxDiscardedFactor(100.0)) { (name: SymbolNode, arguments: Seq[ASTNode], other: SymbolNode) =>
      whenever(name != other) {
        val tested = Associative(name)
        val functionNode = FunctionNode(other, arguments)
        tested.apply(functionNode) shouldBe functionNode
      }
    }
  }

  it("Should be possible if get the same symbol") {
    forAll(sizeRange(10), maxDiscardedFactor(100.0)) { (name: SymbolNode, multiArguments: Seq[Seq[ASTNode]]) =>
      val tested = Associative(name)
      val functionNode = FunctionNode(name, multiArguments.map(FunctionNode(name, _)))
      val actual = tested.apply(functionNode)
      val expected = FunctionNode(name, multiArguments.flatten)
      withClue(f"Size of actual is ${actual.arguments.size} and expected's is ${expected.arguments.size}") {
        actual shouldBe expected
      }
    }
  }
}
