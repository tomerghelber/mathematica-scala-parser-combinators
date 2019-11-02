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
case class PlusMinusNode(expr1: ASTNode, expr2: ASTNode) extends NumberOperationNode
case class SinglePlusMinusNode(expr: ASTNode) extends NumberOperationNode
case class MinusPlusNode(expr1: ASTNode, expr2: ASTNode) extends NumberOperationNode
case class SingleMinusPlusNode(expr: ASTNode) extends NumberOperationNode
case class DivideNode(expr1: ASTNode, expr2: ASTNode) extends NumberOperationNode
case class TimesNode(expr1: ASTNode, expr2: ASTNode) extends NumberOperationNode
case class PlusNode(expr1: ASTNode, expr2: ASTNode) extends NumberOperationNode
