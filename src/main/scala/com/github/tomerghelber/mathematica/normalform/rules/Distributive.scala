package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{FunctionNode, SymbolNode}

/**
 * @author user
 * @since 18-Nov-19
 */
case class Distributive(upper: SymbolNode, lower: SymbolNode) extends NormalFormRuleTemplate {
  require(upper != lower)

  protected def can(node: FunctionNode): Boolean = {
    lower == node.name
  }

  protected def work(node: FunctionNode): FunctionNode = {
    val args = permutations(node.arguments.map{
      case arg: FunctionNode if arg.name == upper => arg.arguments
      case arg => Seq(arg)
    }).map(FunctionNode(lower, _))
    FunctionNode(upper, args)
  }

  private def permutations[T](seqs: Seq[Seq[T]]): Seq[Seq[T]] = {
    seqs match {
      case Nil => Nil
      case head::Nil => Seq(head)
      case head::tail =>
        for (one <- head; permutation <- permutations(tail)) yield {
          one +: permutation
        }
    }
  }
}
