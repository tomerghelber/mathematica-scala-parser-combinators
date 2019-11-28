package com.github.tomerghelber.mathematica.ast

sealed trait ASTNode

final case class FunctionNode(name: SymbolNode, arguments: Seq[ASTNode]) extends ASTNode

sealed trait TerminalNode extends ASTNode {
  def value: String
}

final case class NumberNode(value: String) extends TerminalNode
final case class StringNode(value: String) extends TerminalNode
final case class SymbolNode (value: String) extends TerminalNode
