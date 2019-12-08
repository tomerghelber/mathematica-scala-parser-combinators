package com.github.tomerghelber.mathematica.normalform.rules

import com.github.tomerghelber.mathematica.ast.{ASTNode, FunctionNode, SymbolNode}

/** A normal form rule for distributive functions.
 * @author user
 * @since 18-Nov-19
 */
case class Distributive(upper: SymbolNode, lower: SymbolNode) extends TwoWayRuleTemplate {
  require(upper != lower, f"Lower $lower should be different then upper $upper")

  protected def can(node: FunctionNode): Boolean = lower == node.name

  protected def work(node: FunctionNode): FunctionNode = generalWork(lower, upper, node)

  /** A function to check if the node can be transformed.
   *
   * @param node The node to check if transformable.
   * @return True is the node is transformable and false otherwise.
   */
  override protected def reverseCan(node: FunctionNode): Boolean = upper == node.name

  /** Transforming the node.
   *
   * @param node The node to transform.
   * @return The transformed node.
   */
  override protected def reverseWork(node: FunctionNode): FunctionNode = generalWork(upper, lower, node)

  final private def generalWork(lower: SymbolNode, upper: SymbolNode, node: FunctionNode): FunctionNode = {
    val args = Distributive.permutations(node.arguments.map{
      case arg: FunctionNode if arg.name == upper => arg.arguments
      case arg: ASTNode => Seq(arg)
    }).map(FunctionNode(lower, _))
    FunctionNode(upper, args)
  }
}
object Distributive {
  /** Choosing one element from each sequence to create all permutations.
   * @param seqs The sequences to choose from.
   * @tparam T The type of the elements.
   * @return All permutations
   */
  def permutations[T](seqs: Seq[Seq[T]]): Seq[Seq[T]] = {
    seqs match {
      case Nil => Nil
      case head +: Nil => Seq(head)
      case head +: tail =>
        for (one <- head; permutation <- permutations(tail)) yield {
          one +: permutation
        }
    }
  }
}
