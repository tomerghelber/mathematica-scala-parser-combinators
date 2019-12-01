package com.github.tomerghelber.mathematica.ast

object ImpliesNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "Implies"
}
object RightTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "RightTee"
}
object DoubleRightTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DoubleRightTee"
}
object LeftTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "LeftTee"
}
object DoubleLeftTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DoubleLeftTee"
}
object UpTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "UpTee"
}
object DownTeeNode extends ApplyBinaryFunctionNode {
  override protected val name: String = "DownTee"
}
