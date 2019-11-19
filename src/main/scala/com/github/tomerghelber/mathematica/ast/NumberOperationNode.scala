package com.github.tomerghelber.mathematica.ast

object IncrementNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Increment"
}
object DecrementNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Decrement"
}
object PreincrementNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Preincrement"
}
object PredecrementNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Predecrement"
}
object FactorialNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Factorial"
}
object Factorial2Node extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Factorial2"
}
object PowerNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Power"
}
object SqrtNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Sqrt"
}
object PlusMinusNode extends ApplyUnaryFunctionNode with ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "PlusMinus"
}
object MinusPlusNode extends ApplyUnaryFunctionNode with ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "MinusPlus"
}
object DivideNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Divide"
}
object TimesNode extends ApplyBinaryFunctionNode with ApplyManyFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Times"
}
object PlusNode extends ApplyBinaryFunctionNode with ApplyManyFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Plus"
}
