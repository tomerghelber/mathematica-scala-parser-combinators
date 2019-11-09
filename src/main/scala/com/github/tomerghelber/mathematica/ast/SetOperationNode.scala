package com.github.tomerghelber.mathematica.ast

object IntersectionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Intersection"
}
object UnionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Union"
}
object ElementNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Element"
}
object NotElementNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "NotElement"
}
object SubsetNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Subset"
}
object SupersetNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Superset"
}
