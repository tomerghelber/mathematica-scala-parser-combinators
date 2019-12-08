package com.github.tomerghelber.mathematica.rules

import com.github.tomerghelber.mathematica.ast.FunctionNode

/** A rule to convert a `FunctionNode` to its normal form.
 *
 * @author user
 * @since 18-Nov-19
 */
trait Rule extends (FunctionNode => FunctionNode)
