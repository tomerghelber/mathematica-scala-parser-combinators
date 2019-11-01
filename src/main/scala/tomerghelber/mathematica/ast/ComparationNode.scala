package tomerghelber.mathematica.ast

case class EqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnequalNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class GreaterNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class GreaterEqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class LessNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class LessEqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
