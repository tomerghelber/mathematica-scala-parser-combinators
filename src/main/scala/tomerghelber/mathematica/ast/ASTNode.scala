package tomerghelber.mathematica.ast

trait ASTNode

case class FunctionNode(name: ASTNode, arguments: Seq[ASTNode]) extends ASTNode

case class PartNode(expr: ASTNode, part: ASTNode) extends ASTNode

case class CompositionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class RightCompositionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class DerivativeNode(num: Int, expr: ASTNode) extends ASTNode
case class DifferentialDNode(expr: ASTNode) extends ASTNode
case class IntegrateNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode

case class SameQNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnSameQNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ForAllNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ExistsNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NotExistsNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class EquivalentNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class Implies(expr1: ASTNode, expr2: ASTNode) extends ASTNode

case class LimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class MaxLimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class MinLimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode

case class SpanNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode

trait ApplyUnaryFunctionNode {
  protected def name: String
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode(name), Seq(first))
}
trait ApplyBinaryFunctionNode {
  protected def name: String
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode(name), Seq(first, second))
}
trait UnapplyFunctionNode {
  protected def name: String
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode(name), arguments) => Some(arguments)
    case _ => None
  }
}
