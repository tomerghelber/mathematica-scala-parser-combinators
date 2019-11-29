package com.github.tomerghelber.mathematica.normalform

import com.github.tomerghelber.mathematica.ast.{NumberNode, SymbolNode, TerminalNode}

/** Ordering of `TerminalNode`.
 * @author user
 * @since 18-Nov-19
 */
object TerminalNodeOrdering extends Ordering[TerminalNode] {
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
