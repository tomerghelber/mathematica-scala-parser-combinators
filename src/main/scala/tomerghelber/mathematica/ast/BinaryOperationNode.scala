package tomerghelber.mathematica.ast

object NotNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Not"
}
object AndNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "And"
}
object NandNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Nand"
}
object XorNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Xor"
}
object XnorNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Xnor"
}
object OrNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Or"
}
object NorNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Nor"
}
