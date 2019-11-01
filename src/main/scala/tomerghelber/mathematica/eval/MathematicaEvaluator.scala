package tomerghelber.mathematica.eval

import tomerghelber.mathematica.ast._

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluator {

  def eval(node: ASTNode): ASTNode = {
    node match {
      case node: NumberNode => node
    }
  }
}

