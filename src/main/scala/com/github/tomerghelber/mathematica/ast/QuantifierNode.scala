package com.github.tomerghelber.mathematica.ast

object ForAllNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "ForAll"
}
object ExistsNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Exists"
}
object NotExistsNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "NotExists"
}
object EquivalentNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name = "Equivalent"
}
