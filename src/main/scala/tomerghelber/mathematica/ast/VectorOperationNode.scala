package tomerghelber.mathematica.ast

trait VectorOperationNode extends ASTNode

object CrossNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Cross"
}
object DotNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Dot"
}
