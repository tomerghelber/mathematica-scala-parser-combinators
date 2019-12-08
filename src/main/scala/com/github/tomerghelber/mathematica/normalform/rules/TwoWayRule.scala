package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

trait TwoWayRule extends Rule {
  def reverse(node: FunctionNode): FunctionNode
}
