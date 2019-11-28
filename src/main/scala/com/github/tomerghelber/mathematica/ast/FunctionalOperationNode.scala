package com.github.tomerghelber.mathematica.ast

object PartNode extends ApplyManyFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Part"
}
object CompositionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Composition"
}
object RightCompositionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "RightComposition"
}
