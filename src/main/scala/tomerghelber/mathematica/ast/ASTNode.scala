package tomerghelber.mathematica.ast

trait ASTNode

trait NumberNode extends ASTNode

case class IntegerNode(value: BigInt) extends NumberNode

/**
 * A node for a parsed floating number string. The floating point string is not
 * converted to a binary float or double type. only the String representation is
 * stored.
 */
case class FloatNode(value: Double) extends NumberNode

/**
 * A node for a parsed fraction string
 */
case class FractionNode(numerator: IntegerNode, denominator: IntegerNode) extends NumberNode

/**
 * A list of <code>ASTNode</code>'s which represents a parsed function.
 */
case class FunctionNode(symbol: SymbolNode, args: Seq[ASTNode]) extends ASTNode

case class StringNode(value: String) extends ASTNode

case class SymbolNode (value: String) extends ASTNode
