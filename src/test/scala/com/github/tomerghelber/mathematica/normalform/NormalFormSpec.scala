package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.nodeGen
import org.scalacheck.Arbitrary
import org.scalatest.{FunSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
 * @author user
 * @since 18-Nov-19
 */
class NormalFormSpec extends FunSpec with Matchers with ScalaCheckPropertyChecks {

  private implicit val nodeArbitrary: Arbitrary[ASTNode] = Arbitrary(nodeGen)

  describe("Distribute") {
    it("Plus and Mul") {
      val noramlizer = new NormalForm
      forAll(minSize(2), sizeRange(10), maxDiscardedFactor(100.0)) {
        (multiArguments: Seq[Seq[ASTNode]]) => whenever(multiArguments.nonEmpty) {
          val example = TimesNode(multiArguments.map(arguments => PlusNode(arguments)))

          val result = noramlizer.apply(example)

          noramlizer.apply(result) shouldEqual result

          println("Normal: " + result)
          PlusNode.unapply(result) should not be None

          for (arg <- PlusNode.unapply(result).get) {
            val extracted = TimesNode.unapply(arg)
            extracted should not be None
          }
        }
      }
    }
  }
}
