package tomerghelber.mathematica.ast

trait NumberOperationNode extends ASTNode

case class IncrementNode(expr: ASTNode) extends NumberOperationNode
case class DecrementNode(expr: ASTNode) extends NumberOperationNode
case class PreincrementNode(expr: ASTNode) extends NumberOperationNode
case class PredecrementNode(expr: ASTNode) extends NumberOperationNode
case class FactorialNode(expr: ASTNode) extends NumberOperationNode
case class Factorial2Node(expr: ASTNode) extends NumberOperationNode
case class PowerNode(base: ASTNode, exponent: ASTNode) extends NumberOperationNode
case class SqrtNode(expr: ASTNode) extends NumberOperationNode
object PlusMinusNode {
  def apply(only: ASTNode) = FunctionNode(SymbolNode("PlusMinus"), Seq(only))
  def apply(first: ASTNode, second: ASTNode) = FunctionNode(SymbolNode("PlusMinus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("PlusMinus"), arguments) => Some(arguments)
    case _ => None
  }
}
object MinusPlusNode {
  def apply(only: ASTNode) = FunctionNode(SymbolNode("MinusPlus"), Seq(only))
  def apply(first: ASTNode, second: ASTNode) = FunctionNode(SymbolNode("MinusPlus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("MinusPlus"), arguments) => Some(arguments)
    case _ => None
  }
}
object DivideNode {
  def apply(first: ASTNode, second: ASTNode) = FunctionNode(SymbolNode("Divide"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Divide"), arguments) => Some(arguments)
    case _ => None
  }
}
object TimesNode {
  def apply(first: ASTNode, second: ASTNode) = FunctionNode(SymbolNode("Times"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Times"), arguments) => Some(arguments)
    case _ => None
  }
}
object PlusNode {
  def apply(first: ASTNode, second: ASTNode) = FunctionNode(SymbolNode("Plus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Plus"), arguments) => Some(arguments)
    case _ => None
  }
}
