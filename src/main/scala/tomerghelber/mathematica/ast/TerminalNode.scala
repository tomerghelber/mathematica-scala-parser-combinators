package tomerghelber.mathematica.ast

trait TerminalNode extends ASTNode

case class NumberNode(value: Double) extends TerminalNode
case class StringNode(value: String) extends TerminalNode
case class SymbolNode (value: String) extends TerminalNode
