package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

/**
 * @author user
 * @since 18-Nov-19
 */
trait NormalFormRuleTemplate extends NormalFormRule {
  def apply(node: FunctionNode): FunctionNode = {
    if (can(node)) {
      work(node)
    } else {
      node
    }
  }

  protected def can(node: FunctionNode): Boolean
  protected def work(node: FunctionNode): FunctionNode
}
