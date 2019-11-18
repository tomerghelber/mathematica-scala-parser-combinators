package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{FunctionNode, SymbolNode}

/**
 * @author user
 * @since 18-Nov-19
 */
case class Associative(name: SymbolNode) extends NormalFormRuleTemplate {
  protected def can(node: FunctionNode): Boolean = {
    node.name == name
  }

  protected def work(node: FunctionNode): FunctionNode = {
    val newArgs = node.arguments.flatMap {
      case arg: FunctionNode if arg.name == node.name => arg.arguments
      case arg => Seq(arg)
    }
    node.copy(arguments = newArgs)
  }
}
