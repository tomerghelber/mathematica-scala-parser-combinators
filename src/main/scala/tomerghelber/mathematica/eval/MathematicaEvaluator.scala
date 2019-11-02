package tomerghelber.mathematica.eval

import tomerghelber.mathematica.ast._

import scala.collection.mutable

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluator(globalEnvironment: mutable.Map[String, ASTNode]= mutable.Map.empty[String, ASTNode]) {

  def eval(node: ASTNode): ASTNode = {
    eval(node, globalEnvironment)
  }

  private def eval(node: ASTNode, environment: mutable.Map[String, ASTNode]): ASTNode = {
    node match {
      case node: NumberNode => node
      case SymbolNode(symbol) => environment(symbol)
    }
  }
}

