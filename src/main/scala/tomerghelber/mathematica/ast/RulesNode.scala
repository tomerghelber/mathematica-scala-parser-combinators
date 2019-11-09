package tomerghelber.mathematica.ast

object RuleNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "Rule"
}
object RuleDelayedNode extends ApplyBinaryFunctionNode with UnapplyFunctionNode {
  protected val name: String = "RuleDelayed"
}
