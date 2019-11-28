package com.github.tomerghelber.mathematica.ast

trait ASTNode

final case class FunctionNode(name: SymbolNode, arguments: Seq[ASTNode]) extends ASTNode

sealed trait FunctionNodeSymbol {
  protected val name: String
  lazy val symbol: SymbolNode = SymbolNode(name)
}
trait ApplyUnaryFunctionNode extends FunctionNodeSymbol {
  def apply(node: ASTNode): FunctionNode = createUnary(node)
  val createUnary: ASTNode => FunctionNode = first => FunctionNode(SymbolNode(name), Seq(first))
}
trait ApplyBinaryFunctionNode extends FunctionNodeSymbol {
  def apply(first: ASTNode, second: ASTNode): FunctionNode = createBinary(first, second)
  val createBinary: (ASTNode, ASTNode) => FunctionNode = (first, second) => FunctionNode(SymbolNode(name), Seq(first, second))
}
trait ApplyTernaryFunctionNode extends FunctionNodeSymbol {
  def apply(first: ASTNode, second: ASTNode, third: ASTNode): FunctionNode = createTernary(first, second, third)
  val createTernary: (ASTNode, ASTNode, ASTNode) => FunctionNode =
    (first, second, third) => FunctionNode(SymbolNode(name), Seq(first, second, third))
}
trait ApplyManyFunctionNode extends FunctionNodeSymbol {
  def apply(nodes: Seq[ASTNode]): FunctionNode = createMany(nodes)
  val createMany: Seq[ASTNode] => FunctionNode = nodes => FunctionNode(SymbolNode(name), nodes)
}
trait UnapplyFunctionNode extends FunctionNodeSymbol {
  def unapply(arg: ASTNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(functionSymbol, arguments) if functionSymbol == symbol => Some(arguments)
    case _ => None
  }
}
