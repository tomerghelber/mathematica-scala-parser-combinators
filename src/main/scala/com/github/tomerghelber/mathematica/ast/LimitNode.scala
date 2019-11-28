package com.github.tomerghelber.mathematica.ast

object LimitNode extends ApplyTernaryFunctionNode with UnapplyFunctionNode {
  override protected val name: String = "Limit"
}
object MaxLimitNode extends ApplyTernaryFunctionNode with UnapplyFunctionNode {
  override protected val name: String = "MaxLimit"
}
object MinLimitNode extends ApplyTernaryFunctionNode with UnapplyFunctionNode {
  override protected val name: String = "MinLimit"
}
object SpanNode extends ApplyTernaryFunctionNode with UnapplyFunctionNode {
  override protected val name: String = "Span"
}
