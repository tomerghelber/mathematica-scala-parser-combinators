package com.github.tomerghelber.mathematica.ast

object DNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "D"
}
object DelNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "Del"
}
object DiscreteShiftNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DiscreteShift"
}
object DiscreteRatioNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DiscreteRatio"
}
object DifferenceDeltaNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DifferenceDelta"
}
