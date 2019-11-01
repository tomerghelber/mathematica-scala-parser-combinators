package tomerghelber.mathematica.ast

case class IntersectionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ElementNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NotElementNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SubsetNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SupersetNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
