package tomerghelber.mathematica.ast

trait ScriptNode extends ASTNode

object MessageNameNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "MessageName"
}
object OverscriptNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Overscript"
}
object UnderscriptNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Underscript"
}
object SubscriptNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Subscript"
}
