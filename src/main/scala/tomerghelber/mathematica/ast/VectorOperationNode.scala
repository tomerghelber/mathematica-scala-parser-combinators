package tomerghelber.mathematica.ast

trait VectorOperationNode extends ASTNode

case class CrossNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends VectorOperationNode
case class DotNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends VectorOperationNode
