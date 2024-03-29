package com.github.tomerghelber.mathematica.rules.normalform

import com.github.tomerghelber.mathematica.ast.{FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.normalform.NormalForm.ASTNodeOrdering
import com.github.tomerghelber.mathematica.rules.RuleTemplate

/** A normal form rule for commutative functions.
 * @author user
 * @since 18-Nov-19
 */
case class Commutative(name: SymbolNode) extends RuleTemplate {
  protected def can(node: FunctionNode): Boolean = node.name == name

  protected def work(node: FunctionNode): FunctionNode =
    node.copy(arguments = node.arguments.toList.sorted)
}
