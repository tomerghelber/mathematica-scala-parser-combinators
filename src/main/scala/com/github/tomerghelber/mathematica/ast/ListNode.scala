package com.github.tomerghelber.mathematica.ast

object ListNode extends ApplyManyFunctionNode with UnapplyFunctionNode {
  protected val name = "List"
}
