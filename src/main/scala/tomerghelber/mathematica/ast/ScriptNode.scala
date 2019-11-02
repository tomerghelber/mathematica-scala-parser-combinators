package tomerghelber.mathematica.ast

trait ScriptNode extends ASTNode

case class MessageNameNode(symbol: ASTNode, tag: StringNode) extends ScriptNode
case class OverscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode
case class UnderscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode
case class SubscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode
