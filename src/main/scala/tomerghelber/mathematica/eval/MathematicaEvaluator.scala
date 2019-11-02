package tomerghelber.mathematica.eval

import com.typesafe.scalalogging.LazyLogging
import tomerghelber.mathematica.ast._

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
    node match {
      case NumberNode(fraction) if fraction.contains("/") =>
        NumberNode(fraction.split("/").map(_.toDouble).reduce(_ / _).toString)
      case node: NumberNode => node
      case node: StringNode => node
      case SymbolNode(symbol) => environment(symbol)
      case PlusNode(arguments) =>
        NumberNode(arguments.map(eval(_, environment).asInstanceOf[NumberNode]).map(_.value.toDouble).sum.toString)
      case TimesNode(arguments) =>
        NumberNode(arguments.map(eval(_, environment).asInstanceOf[NumberNode]).map(_.value.toDouble).product.toString)
      case DivideNode(Seq(first, second)) =>
        (eval(first), eval(second)) match {
          case (NumberNode(a) , NumberNode(b)) => NumberNode((a.toDouble / b.toDouble).toString)
          case other => throw new MatchError(other)
        }
      case other => throw new MatchError(other)
    }
  }
}

