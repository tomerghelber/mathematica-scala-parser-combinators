package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.normalform.rules.{Associative, Commutative, Distributive, NormalFormRule}

/** Normal form transformer.
 * @author user
 * @since 18-Nov-19
 */
class NormalForm {
  private val rules: Set[NormalFormRule] = Set(
    // Distributives
    Distributive(PlusNode.symbol, TimesNode.symbol),
    Distributive(OrNode.symbol, AndNode.symbol),
    // Associatives
    Associative(OrNode.symbol),
    Associative(AndNode.symbol),
    Associative(XorNode.symbol),
    Associative(PlusNode.symbol),
    Associative(TimesNode.symbol),
    // Commutatives
    Commutative(OrNode.symbol),
    Commutative(AndNode.symbol),
    Commutative(NorNode.symbol),
    Commutative(NandNode.symbol),
    Commutative(XorNode.symbol),
    Commutative(XnorNode.symbol),
    Commutative(PlusNode.symbol),
    Commutative(TimesNode.symbol),
  )

  def apply(node: ASTNode): ASTNode = {
    node match {
      case terminal: TerminalNode => terminal
      case FunctionNode(name, arguments) =>
        var newNode = FunctionNode(name, arguments.map(apply))
        var lastNode = node
        while (lastNode != newNode) {
          lastNode = newNode
          newNode = rules.foldRight(newNode)(_ apply _)
        }
        newNode
    }
  }
}
object NormalForm {
  /** Ordering of `TerminalNode`.
   * @author user
   * @since 18-Nov-19
   */
  implicit object TerminalNodeOrdering extends Ordering[TerminalNode] {
    override def compare(x: TerminalNode, y: TerminalNode): Int = {
      (x, y) match {
        case (x, y) if x.getClass == y.getClass => Ordering.String.compare(x.value, y.value)
        case (_: NumberNode, _) => -1
        case (_, _: NumberNode) => 1
        case (_, _: SymbolNode) => -1
        case (_: SymbolNode, _) => 1
      }
    }
  }

  /** Ordering of `ASTNode`.
   * @author user
   * @since 18-Nov-19
   */
  implicit object ASTNodeOrdering extends Ordering[ASTNode] {
    override def compare(x: ASTNode, y: ASTNode): Int = {
      (x, y) match {
        case (x: TerminalNode, y: TerminalNode) => TerminalNodeOrdering.compare(x, y)
        case (x: FunctionNode, y: FunctionNode) => normalFormFunctionNodeOrdering.compare(x, y)
        case (_: TerminalNode, _: FunctionNode) => -1
        case (_: FunctionNode, _: TerminalNode) => 1
      }
    }
    private val normalFormFunctionNodeOrdering = {
      val functionNameOrdering = Ordering.by[FunctionNode, TerminalNode](_.name)
      val functionArgumentsOrdering =
        Ordering.by[FunctionNode, Iterable[ASTNode]](_.arguments)(Ordering.Iterable[ASTNode])
      functionNameOrdering thenComparing functionArgumentsOrdering
    }
  }

}
