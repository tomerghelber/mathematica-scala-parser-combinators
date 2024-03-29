package com.github.tomerghelber.mathematica.ast

object ConjugateNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Conjugate"
}
object TransposeNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Transpose"
}
object ConjugateTransposeNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "ConjugateTranspose"
}
