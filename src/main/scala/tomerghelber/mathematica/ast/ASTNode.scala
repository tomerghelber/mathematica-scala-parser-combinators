package tomerghelber.mathematica.ast

trait ASTNode

case class FunctionNode(name: ASTNode, arguments: Seq[ASTNode]) extends ASTNode

object PartNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Part"
}

object CompositionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Composition"
}
object RightCompositionNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "RightComposition"
}
case class DerivativeNode(num: Int, expr: ASTNode) extends ASTNode
object DifferentialDNode extends ApplyUnaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "DifferentialD"
}
object IntegrateNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Integrate"
}

object SameQNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "SameQ"
}
object UnSameQNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "UnSameQ"
}
object ForAllNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "ForAll"
}
object ExistsNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Exists"
}
object NotExistsNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "NotExists"
}
object EquivalentNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name = "Equivalent"
}
object ListNode extends UnapplyFunctionNode {
  protected val name = "List"
}

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
    case FunctionNode(SymbolNode(functionName), arguments) if functionName == name => Some(arguments)
    case _ => None
  }
}
