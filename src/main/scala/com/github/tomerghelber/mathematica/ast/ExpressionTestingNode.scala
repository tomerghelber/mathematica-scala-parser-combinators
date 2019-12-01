package com.github.tomerghelber.mathematica.ast

object SameQNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "SameQ"
}
object UnSameQNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "UnSameQ"
}
