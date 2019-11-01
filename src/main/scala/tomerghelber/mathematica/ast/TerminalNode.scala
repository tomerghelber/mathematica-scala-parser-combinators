package tomerghelber.mathematica.ast

case class NumberNode(value: Double) extends ASTNode
case class StringNode(value: String) extends ASTNode
case class SymbolNode (value: String) extends ASTNode
