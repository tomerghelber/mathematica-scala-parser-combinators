package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.normalform.rules.{Associative, Distributive, NormalFormRule}

/**
 * @author user
 * @since 18-Nov-19
 */
class NormalForm {
  private val rules: Set[NormalFormRule] = Set(
    Distributive(SymbolNode("Plus"), SymbolNode("Mul")),
    Associative(SymbolNode("Plus")),
    Associative(SymbolNode("Mul")),
  )

  def apply(node: ASTNode): ASTNode = {
    node match {
      case terminal: TerminalNode => terminal
      case FunctionNode(name, arguments) =>
        var newNode = FunctionNode(name, arguments.map(apply))
        var lastNode = node
        while (lastNode != newNode) {
          lastNode = newNode
          newNode = rules.foldRight(newNode)(_ apply _)
        }
        newNode
    }
  }
}
