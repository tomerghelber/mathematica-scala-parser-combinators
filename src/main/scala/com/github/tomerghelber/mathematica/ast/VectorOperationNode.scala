package com.github.tomerghelber.mathematica.ast

object CrossNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Cross"
}
object DotNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Dot"
}
