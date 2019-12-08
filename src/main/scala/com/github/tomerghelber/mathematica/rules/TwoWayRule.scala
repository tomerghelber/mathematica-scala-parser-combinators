package com.github.tomerghelber.mathematica.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

trait TwoWayRule extends Rule {
  def reverse(node: FunctionNode): FunctionNode
}
