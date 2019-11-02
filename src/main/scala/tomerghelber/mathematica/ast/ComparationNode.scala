package tomerghelber.mathematica.ast

object EqualNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Equal"
}
object UnequalNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Unequal"
}
object GreaterNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Greater"
}
object GreaterEqualNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "GreaterEqual"
}
object LessNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Less"
}
object LessEqualNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "LessEqual"
}
