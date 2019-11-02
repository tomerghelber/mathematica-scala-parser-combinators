package tomerghelber.mathematica.ast

trait SetOperationNode extends ASTNode

case class IntersectionNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
case class UnionNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
case class ElementNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
case class NotElementNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
case class SubsetNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
case class SupersetNode(expr1: ASTNode, expr2: ASTNode) extends SetOperationNode
