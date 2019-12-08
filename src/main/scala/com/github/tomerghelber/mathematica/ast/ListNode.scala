package com.github.tomerghelber.mathematica.ast

object ListNode extends ApplyManyFunctionNode with UnapplyFunctionNode {
  protected val name = "List"
}
object StringJoinNode extends ApplyTernaryFunctionNode with UnapplyFunctionNode {
  protected val name = "StringJoin"
}
