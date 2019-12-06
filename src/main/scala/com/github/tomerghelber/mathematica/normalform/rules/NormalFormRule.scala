package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

trait Rule extends (FunctionNode => FunctionNode)
trait TwoWayRule extends Rule {
  def reverse(node: FunctionNode): FunctionNode
}

/** A rule to convert a `FunctionNode` to its normal form.
 * @author user
 * @since 18-Nov-19
 */
trait NormalFormRule extends Rule
