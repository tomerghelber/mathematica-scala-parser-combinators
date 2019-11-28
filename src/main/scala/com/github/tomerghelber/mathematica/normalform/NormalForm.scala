package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.normalform.rules.{Associative, Commutative, Distributive, NormalFormRule}

/**
 * @author user
 * @since 18-Nov-19
 */
class NormalForm {
  private val rules: Set[NormalFormRule] = Set(
    // Distributives
    Distributive(PlusNode.symbol, TimesNode.symbol),
    Distributive(OrNode.symbol, AndNode.symbol),
    // Associatives
    Associative(OrNode.symbol),
    Associative(AndNode.symbol),
    Associative(XorNode.symbol),
    Associative(PlusNode.symbol),
    Associative(TimesNode.symbol),
    // Commutatives
    Commutative(OrNode.symbol),
    Commutative(AndNode.symbol),
    Commutative(NorNode.symbol),
    Commutative(NandNode.symbol),
    Commutative(XorNode.symbol),
    Commutative(XnorNode.symbol),
    Commutative(PlusNode.symbol),
    Commutative(TimesNode.symbol),
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
