package tomerghelber.mathematica.ast

trait ASTNode

trait NumberNode extends ASTNode
trait ScriptNode extends ASTNode

case class IntegerNode(value: BigInt) extends NumberNode
case class FloatNode(value: Double) extends NumberNode
case class StringNode(value: String) extends ASTNode
case class SymbolNode (value: String) extends ASTNode

case class MessageNameNode(symbol: ASTNode, tag: StringNode) extends ASTNode
case class OverscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode
case class UnderscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode
case class SubscriptNode(expr1: ASTNode, expr2: ASTNode) extends ScriptNode

case class PartNode(expr: ASTNode, parts: List[ASTNode]) extends ASTNode

case class IncrementNode(expr: ASTNode) extends ASTNode
case class DecrementNode(expr: ASTNode) extends ASTNode
case class PreincrementNode(expr: ASTNode) extends ASTNode
case class PredecrementNode(expr: ASTNode) extends ASTNode
case class CompositionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class RightCompositionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class MapNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class MapAllNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class Apply2Node(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class Apply3Node(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class FactorialNode(expr: ASTNode) extends ASTNode
case class Factorial2Node(expr: ASTNode) extends ASTNode
case class ConjugateNode(expr: ASTNode) extends ASTNode
case class TransposeNode(expr: ASTNode) extends ASTNode
case class ConjugateTransposeNode(expr: ASTNode) extends ASTNode
case class DerivativeNode(num: Int, expr: ASTNode) extends ASTNode
case class StringJoinNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class PowerNode(base: ASTNode, exponent: ASTNode) extends ASTNode
case class SqrtNode(expr: ASTNode) extends ASTNode
case class DifferentialDNode(expr: ASTNode) extends ASTNode
case class DNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class DelNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class DiscreteShiftNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class DiscreteRatioNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class DifferenceDeltaNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class CrossNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class DotNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class PlusMinusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SinglePlusMinusNode(expr: ASTNode) extends ASTNode
case class MinusPlusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SingleMinusPlusNode(expr: ASTNode) extends ASTNode
case class DivideNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class TimesNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class PlusNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class IntegrateNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class IntersectionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnionNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode

case class EqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnequalNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class GreaterNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class GreaterEqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class LessNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class LessEqualNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode

case class SameQNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class UnSameQNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ElementNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NotElementNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SubsetNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class SupersetNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ForAllNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class ExistsNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NotExistsNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NotNode(expr: ASTNode) extends ASTNode
case class AndNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NandNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class XorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class XnorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class OrNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class NorNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class EquivalentNode(expr1: ASTNode, expr2: ASTNode) extends ASTNode
case class Implies(expr1: ASTNode, expr2: ASTNode) extends ASTNode

case class LimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class MaxLimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode
case class MinLimitNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode

case class SpanNode(expr1: ASTNode, expr3: ASTNode, expr2: ASTNode) extends ASTNode