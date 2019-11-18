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
      case (x: FunctionNode, y: FunctionNode) => NormalFormFunctionNodeOrdering.compare(x, y)
      case (_: TerminalNode, _: FunctionNode) => -1
      case (_: FunctionNode, _: TerminalNode) => 1
      case (_: Any, _: Any) => throw new MatchError((x, y))
    }
  }
  private val NormalFormFunctionNodeOrdering =
      Ordering.by[FunctionNode, TerminalNode](_.name)(TerminalNodeOrdering).
        thenComparing(Ordering.by[FunctionNode, Iterable[ASTNode]](_.arguments)(Ordering.Iterable[ASTNode](ASTNodeOrdering)))
}
