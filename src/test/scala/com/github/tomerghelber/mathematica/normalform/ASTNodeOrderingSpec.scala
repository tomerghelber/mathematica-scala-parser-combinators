package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode, TerminalNode}
import com.github.tomerghelber.mathematica.normalform.NormalForm.{ASTNodeOrdering, TerminalNodeOrdering}
import com.github.tomerghelber.mathematica.{functionNodeGen, nodeGen, symbolNodeGen, terminalNodeGen}
import org.scalacheck.Arbitrary
import org.scalatest.matchers.should.Matchers
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * @author user
 * @since 18-Nov-19
 */
class ASTNodeOrderingSpec extends AnyFunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)
  private implicit val functionNodeArbitrary: Arbitrary[FunctionNode] = Arbitrary(functionNodeGen)
  private implicit val symbolNodeArbitrary: Arbitrary[SymbolNode] = Arbitrary(symbolNodeGen)
  private implicit val terminalNodeArbitrary: Arbitrary[TerminalNode] = Arbitrary(terminalNodeGen)

  it("Terminal should be ordered before functions") {
    forAll { (function: FunctionNode, terminal: TerminalNode) =>
      ASTNodeOrdering.compare(terminal, function) shouldEqual -1
      ASTNodeOrdering.compare(function, terminal) shouldEqual 1
    }
  }

  it("Terminal should be ordered as terminal order functions") {
    forAll { (terminal1: TerminalNode, terminal2: TerminalNode) =>
      ASTNodeOrdering.compare(terminal1, terminal2) shouldEqual TerminalNodeOrdering.compare(terminal1, terminal2)
      ASTNodeOrdering.compare(terminal2, terminal1) shouldEqual TerminalNodeOrdering.compare(terminal2, terminal1)
    }
  }

  it("Symbols should be used first to order functions") {
    forAll(sizeRange(10)) {
      (name1: SymbolNode, name2: SymbolNode, arguments1: Seq[ASTNode], arguments2: Seq[ASTNode]) =>
      val f1 = FunctionNode(name1, arguments1)
      val f2 = FunctionNode(name2, arguments2)
      ASTNodeOrdering.compare(f1, f2) shouldEqual ASTNodeOrdering.compare(name1, name2)
      ASTNodeOrdering.compare(f2, f1) shouldEqual ASTNodeOrdering.compare(name2, name1)
    }
  }

  it("First inner arguments should influence order") {
    forAll(sizeRange(10)) { (name: SymbolNode, additionalNode1: ASTNode, additionalNode2: ASTNode,
       arguments1: Seq[ASTNode], arguments2: Seq[ASTNode]) =>
      val f1 = FunctionNode(name, additionalNode1 +: arguments1)
      val f2 = FunctionNode(name, additionalNode2 +: arguments2)
      ASTNodeOrdering.compare(f1, f2) shouldEqual ASTNodeOrdering.compare(additionalNode1, additionalNode2)
      ASTNodeOrdering.compare(f2, f1) shouldEqual ASTNodeOrdering.compare(additionalNode2, additionalNode1)
    }
  }

  it("Middle inner arguments should influence order") {
    forAll(sizeRange(10)) {
      (name: SymbolNode, additionalNode1: ASTNode, additionalNode2: ASTNode, arguments: Seq[ASTNode]) =>
      val f1 = FunctionNode(name, arguments ++ (additionalNode1 +: arguments))
      val f2 = FunctionNode(name, arguments ++ (additionalNode2 +: arguments))
      ASTNodeOrdering.compare(f1, f2) shouldEqual ASTNodeOrdering.compare(additionalNode1, additionalNode2)
      ASTNodeOrdering.compare(f2, f1) shouldEqual ASTNodeOrdering.compare(additionalNode2, additionalNode1)
    }
  }

  it("Last inner arguments should influence order") {
    forAll(sizeRange(10)) {
      (name: SymbolNode, additionalNode1: ASTNode, additionalNode2: ASTNode, arguments: Seq[ASTNode]) =>
      val f1 = FunctionNode(name, arguments :+ additionalNode1)
      val f2 = FunctionNode(name, arguments :+ additionalNode2)
      ASTNodeOrdering.compare(f1, f2) shouldEqual ASTNodeOrdering.compare(additionalNode1, additionalNode2)
      ASTNodeOrdering.compare(f2, f1) shouldEqual ASTNodeOrdering.compare(additionalNode2, additionalNode1)
    }
  }

  it("Longer argument should be order after shorter") {
    forAll(sizeRange(10)) { (name: SymbolNode, additionalNode: ASTNode, arguments: Seq[ASTNode]) =>
      val f1 = FunctionNode(name, arguments)
      val f2 = FunctionNode(name, arguments :+ additionalNode)
      ASTNodeOrdering.compare(f1, f2) shouldEqual -1
      ASTNodeOrdering.compare(f2, f1) shouldEqual 1
    }
  }
}
