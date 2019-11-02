package tomerghelber.mathematica.ast

trait MatrixOperationNode extends ASTNode

case class ConjugateNode(expr: ASTNode) extends MatrixOperationNode
case class TransposeNode(expr: ASTNode) extends MatrixOperationNode
case class ConjugateTransposeNode(expr: ASTNode) extends MatrixOperationNode
