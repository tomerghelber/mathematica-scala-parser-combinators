package com.github.tomerghelber.mathematica.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode
import com.github.tomerghelber.mathematica.functionNodeGen
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RuleTemplateSpec extends AsyncFunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val functionNodeArbitrary: Arbitrary[FunctionNode] = Arbitrary(functionNodeGen)

  private class RuleTemplateTest(isCan: Boolean) extends RuleTemplate {
    var insertedToWork = 0
    override protected def can(node: FunctionNode): Boolean = isCan
    override protected def work(node: FunctionNode): FunctionNode = {
      insertedToWork += 1
      node
    }
  }

  it("If can is true the do work") {
    forAll { functionNode: FunctionNode =>
      val tested = new RuleTemplateTest(true)

      tested.apply(functionNode)

      tested.insertedToWork shouldBe 1
    }
  }

  it("If can is false then returns the same value") {
    forAll { functionNode: FunctionNode =>
      val tested = new RuleTemplateTest(false)

      val result = tested.apply(functionNode)

      tested.insertedToWork shouldBe 0
      result should be theSameInstanceAs functionNode
    }
  }
}
