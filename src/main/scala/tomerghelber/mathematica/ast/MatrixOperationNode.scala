package tomerghelber.mathematica.ast

case class ConjugateNode(expr: ASTNode) extends ASTNode
case class TransposeNode(expr: ASTNode) extends ASTNode
case class ConjugateTransposeNode(expr: ASTNode) extends ASTNode
