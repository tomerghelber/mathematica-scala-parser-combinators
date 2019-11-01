package tomerghelber.mathematica.ast

case class NotNode(expr: ASTNode) extends ASTNode
case class AndNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NandNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class XorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class XnorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class OrNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
