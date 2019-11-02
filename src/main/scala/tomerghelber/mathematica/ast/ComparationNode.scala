package tomerghelber.mathematica.ast

trait ComparationNode extends ASTNode

case class EqualNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
case class UnequalNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
case class GreaterNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
case class GreaterEqualNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
case class LessNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
case class LessEqualNode(expr1: ASTNode, expr2: ASTNode) extends ComparationNode
