package com.github.tomerghelber.mathematica.rules.normalform

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode}
import com.github.tomerghelber.mathematica.rules.RuleTemplate

/** A normal form rule for associative functions.
 * @author user
 * @since 18-Nov-19
 */
case class Associative(name: SymbolNode) extends RuleTemplate {
  protected def can(node: FunctionNode): Boolean = node.name == name

  protected def work(node: FunctionNode): FunctionNode = {
    val newArgs = node.arguments.flatMap {
      case arg: FunctionNode if arg.name == node.name => arg.arguments
      case arg: ASTNode => Some(arg)
    }
    node.copy(arguments = newArgs)
  }
}
