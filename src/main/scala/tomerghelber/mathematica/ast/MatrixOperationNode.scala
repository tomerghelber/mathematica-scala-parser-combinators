package tomerghelber.mathematica.ast

trait MatrixOperationNode extends ASTNode

object ConjugateNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Conjugate"
}
object TransposeNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Transpose"
}
object ConjugateTransposeNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "ConjugateTranspose"
}
