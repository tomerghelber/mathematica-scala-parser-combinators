package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.normalform.ASTNodeOrdering

/**
 * @author user
 * @since 18-Nov-19
 */
case class Commutative(name: SymbolNode) extends NormalFormRuleTemplate {
  protected def can(node: FunctionNode): Boolean = {
    node.name == name
  }

  protected def work(node: FunctionNode): FunctionNode = {
    node.copy(arguments = node.arguments.toList.sorted(ASTNodeOrdering))
  }
}