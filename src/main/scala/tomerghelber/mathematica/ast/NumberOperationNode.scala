package tomerghelber.mathematica.ast

case class IncrementNode(expr: ASTNode) extends ASTNode
case class DecrementNode(expr: ASTNode) extends ASTNode
case class PreincrementNode(expr: ASTNode) extends ASTNode
case class PredecrementNode(expr: ASTNode) extends ASTNode
case class FactorialNode(expr: ASTNode) extends ASTNode
case class Factorial2Node(expr: ASTNode) extends ASTNode
case class PowerNode(base: ASTNode, exponent: ASTNode) extends ASTNode
case class SqrtNode(expr: ASTNode) extends ASTNode
case class PlusMinusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SinglePlusMinusNode(expr: ASTNode) extends ASTNode
case class MinusPlusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SingleMinusPlusNode(expr: ASTNode) extends ASTNode
case class DivideNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class TimesNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class PlusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
