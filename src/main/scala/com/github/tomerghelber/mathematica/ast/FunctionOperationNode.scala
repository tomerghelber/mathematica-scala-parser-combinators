package com.github.tomerghelber.mathematica.ast

import scala.util.Try

object DerivativeNode {
  private val name: String = "Derivative"
  lazy val symbol: SymbolNode = SymbolNode(name)

  def apply(num: Int, expr: ASTNode): FunctionNode = create(num, expr)

  def create: (Int, ASTNode) => FunctionNode =
    (num: Int, expr: ASTNode) => FunctionNode(symbol, Seq(NumberNode(num.toString), expr))

  def unapply(arg: ASTNode): Option[(Int, ASTNode)] = arg match {
    case FunctionNode(functionSymbol, Seq(NumberNode(num), expr))
      if functionSymbol == symbol && Try(num.toInt).isSuccess =>
      Some((num.toInt, expr))
    case _ => None
  }
}
object DifferentialDNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "DifferentialD"
}
object IntegrateNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Integrate"
}
