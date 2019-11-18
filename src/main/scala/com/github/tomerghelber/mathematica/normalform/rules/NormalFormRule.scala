package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

/**
 * @author user
 * @since 18-Nov-19
 */
trait NormalFormRule {
  def apply(node: FunctionNode): FunctionNode
}
