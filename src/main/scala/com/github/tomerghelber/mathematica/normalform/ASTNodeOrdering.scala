package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, TerminalNode}

/**
 * @author user
 * @since 18-Nov-19
 */
object ASTNodeOrdering extends Ordering[ASTNode] {
  override def compare(x: ASTNode, y: ASTNode): Int = {
    (x, y) match {
      case (x: TerminalNode, y: TerminalNode) => TerminalNodeOrdering.compare(x, y)
      case (x: FunctionNode, y: FunctionNode) => normalFormFunctionNodeOrdering.compare(x, y)
      case (_: TerminalNode, _: FunctionNode) => -1
      case (_: FunctionNode, _: TerminalNode) => 1
    }
  }
  private val normalFormFunctionNodeOrdering = {
    val functionNameOrdering = Ordering.by[FunctionNode, TerminalNode](_.name)(TerminalNodeOrdering)
    val functionArgumentsOrdering =
      Ordering.by[FunctionNode, Iterable[ASTNode]](_.arguments)(Ordering.Iterable[ASTNode](ASTNodeOrdering))
    functionNameOrdering thenComparing functionArgumentsOrdering
  }
}
