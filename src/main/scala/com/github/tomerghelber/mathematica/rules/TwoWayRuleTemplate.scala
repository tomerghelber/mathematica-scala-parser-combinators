package com.github.tomerghelber.mathematica.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

trait TwoWayRuleTemplate extends RuleTemplate with TwoWayRule {
  final override def reverse(node: FunctionNode): FunctionNode = {
    if (reverseCan(node)) {
      reverseWork(node)
    } else {
      node
    }
  }

  /** A function to check if the node can be transformed.
   *
   * @param node The node to check if transformable.
   * @return True is the node is transformable and false otherwise.
   */
  protected def reverseCan(node: FunctionNode): Boolean

  /** Transforming the node.
   *
   * @param node The node to transform.
   * @return The transformed node.
   */
  protected def reverseWork(node: FunctionNode): FunctionNode
}
