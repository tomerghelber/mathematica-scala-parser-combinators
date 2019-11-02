package tomerghelber.mathematica.ast

trait BinaryOperationNode extends ASTNode

case class NotNode(expr: ASTNode) extends BinaryOperationNode
case class AndNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
case class NandNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
case class XorNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
case class XnorNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
case class OrNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
case class NorNode(expr1: ASTNode, expr2: ASTNode) extends BinaryOperationNode
