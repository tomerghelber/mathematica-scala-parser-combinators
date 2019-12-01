package com.github.tomerghelber.mathematica.ast

object NotNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Not"
}
object AndNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "And"
}
object NandNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Nand"
}
object XorNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Xor"
}
object XnorNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Xnor"
}
object OrNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Or"
}
object NorNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Nor"
}
