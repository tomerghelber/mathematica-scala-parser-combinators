package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

/** A template of basic normal forms.
 * @author user
 * @since 18-Nov-19
 */
trait RuleTemplate extends Rule {
  final override def apply(node: FunctionNode): FunctionNode = {
    if (can(node)) {
      work(node)
    } else {
      node
    }
  }

  /** A function to check if the node can be transformed.
   * @param node The node to check if transformable.
   * @return True is the node is transformable and false otherwise.
   */
  protected def can(node: FunctionNode): Boolean

  /** Transforming the node.
   * @param node The node to transform.
   * @return The transformed node.
   */
  protected def work(node: FunctionNode): FunctionNode
}
