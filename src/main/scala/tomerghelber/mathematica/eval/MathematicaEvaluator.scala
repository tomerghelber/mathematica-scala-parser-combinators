package tomerghelber.mathematica.eval

import com.typesafe.scalalogging.LazyLogging
import tomerghelber.mathematica.ast.ASTNode

import scala.collection.mutable

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluator(globalEnvironment: mutable.Map[String, ASTNode]= mutable.Map.empty[String, ASTNode])
  extends LazyLogging {

  def eval(node: ASTNode): ASTNode = {
    logger.debug("Evalutating " + node)
    eval(node, globalEnvironment)
  }

  private def eval(node: ASTNode, environment: mutable.Map[String, ASTNode]): ASTNode = {
    import tomerghelber.mathematica.ast._
    node match {
      case node: NumberNode => node
      case node: StringNode => node
      case SymbolNode(symbol) => environment(symbol)
//      case FunctionNode(name, arguments) =>
//        logger.debug("Unknown function")
//        environment(name) match {
//          case
//        }
    }
  }
}

