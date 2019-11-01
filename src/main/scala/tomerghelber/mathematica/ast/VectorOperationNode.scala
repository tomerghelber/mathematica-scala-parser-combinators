package tomerghelber.mathematica.ast

case class CrossNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class DotNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
