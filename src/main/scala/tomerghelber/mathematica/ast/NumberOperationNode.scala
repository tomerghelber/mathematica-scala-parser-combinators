package tomerghelber.mathematica.ast

object IncrementNode {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Increment"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Increment"), arguments) => Some(arguments)
    case _ => None
  }
}
object DecrementNode {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Decrement"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Decrement"), arguments) => Some(arguments)
    case _ => None
  }
}
object PreincrementNode {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Preincrement"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Preincrement"), arguments) => Some(arguments)
    case _ => None
  }
}
object PredecrementNode {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Predecrement"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Predecrement"), arguments) => Some(arguments)
    case _ => None
  }
}
object FactorialNode {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Factorial"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Factorial"), arguments) => Some(arguments)
    case _ => None
  }
}
object Factorial2Node {
  def apply(first: ASTNode): FunctionNode = FunctionNode(SymbolNode("Factorial2"), Seq(first))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Factorial2"), arguments) => Some(arguments)
    case _ => None
  }
}
object PowerNode {
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("Power"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Power"), arguments) => Some(arguments)
    case _ => None
  }
}
object SqrtNode {
  def apply(only: ASTNode): FunctionNode = FunctionNode(SymbolNode("Sqrt"), Seq(only))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Sqrt"), arguments) => Some(arguments)
    case _ => None
  }
}
object PlusMinusNode {
  def apply(only: ASTNode): FunctionNode = FunctionNode(SymbolNode("PlusMinus"), Seq(only))
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("PlusMinus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("PlusMinus"), arguments) => Some(arguments)
    case _ => None
  }
}
object MinusPlusNode {
  def apply(only: ASTNode): FunctionNode = FunctionNode(SymbolNode("MinusPlus"), Seq(only))
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("MinusPlus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("MinusPlus"), arguments) => Some(arguments)
    case _ => None
  }
}
object DivideNode {
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("Divide"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Divide"), arguments) => Some(arguments)
    case _ => None
  }
}
object TimesNode {
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("Times"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Times"), arguments) => Some(arguments)
    case _ => None
  }
}
object PlusNode {
  def apply(first: ASTNode, second: ASTNode): FunctionNode = FunctionNode(SymbolNode("Plus"), Seq(first, second))
  def unapply(arg: FunctionNode): Option[Seq[ASTNode]] = arg match {
    case FunctionNode(SymbolNode("Plus"), arguments) => Some(arguments)
    case _ => None
  }
}
