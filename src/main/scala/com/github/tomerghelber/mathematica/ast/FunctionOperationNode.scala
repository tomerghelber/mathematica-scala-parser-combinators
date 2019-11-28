package com.github.tomerghelber.mathematica.ast

case class DerivativeNode(num: Int, expr: ASTNode) extends ASTNode
object DifferentialDNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "DifferentialD"
}
object IntegrateNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Integrate"
}
