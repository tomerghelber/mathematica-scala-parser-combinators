package com.github.tomerghelber.mathematica.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode
import com.github.tomerghelber.mathematica.functionNodeGen
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TwoWayRuleTemplateSpec extends AsyncFunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val functionNodeArbitrary: Arbitrary[FunctionNode] = Arbitrary(functionNodeGen)

  private class TwoWayRuleTemplateTest(isReverseCan: Boolean) extends TwoWayRuleTemplate {
    var insertedToReverseWork = 0
    override protected def reverseCan(node: FunctionNode): Boolean = isReverseCan
    override protected def reverseWork(node: FunctionNode): FunctionNode = {
      insertedToReverseWork += 1
      node
    }

    /** A function to check if the node can be transformed.
     *
     * @param node The node to check if transformable.
     * @return True is the node is transformable and false otherwise.
     */
    override protected def can(node: FunctionNode): Boolean = ???

    /** Transforming the node.
     *
     * @param node The node to transform.
     * @return The transformed node.
     */
    override protected def work(node: FunctionNode): FunctionNode = ???
  }

  it("If reverseCan is true the do reverseWork") {
    forAll { functionNode: FunctionNode =>
      val tested = new TwoWayRuleTemplateTest(true)

      tested.apply(functionNode)

      tested.insertedToReverseWork shouldBe 1
    }
  }

  it("If reverseCan is false then returns the same value") {
    forAll { functionNode: FunctionNode =>
      val tested = new TwoWayRuleTemplateTest(false)

      val result = tested.apply(functionNode)

      tested.insertedToReverseWork shouldBe 0
      result should be theSameInstanceAs functionNode
    }
  }
}
