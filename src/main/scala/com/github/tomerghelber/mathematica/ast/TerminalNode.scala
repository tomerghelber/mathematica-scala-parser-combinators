package com.github.tomerghelber.mathematica.ast

sealed trait TerminalNode extends ASTNode {
  def value: String
}

case class NumberNode(value: String) extends TerminalNode
case class StringNode(value: String) extends TerminalNode
case class SymbolNode (value: String) extends TerminalNode
