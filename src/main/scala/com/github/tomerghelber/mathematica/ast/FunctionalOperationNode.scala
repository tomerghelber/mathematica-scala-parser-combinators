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
object MapNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Map"
}
object MapAllNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "MapAll"
}
object Apply2Node extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Apply2"
}
object Apply3Node extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Apply3"
}
