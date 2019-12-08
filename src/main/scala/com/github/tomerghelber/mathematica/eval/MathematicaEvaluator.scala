package com.github.tomerghelber.mathematica.eval

import com.github.tomerghelber.mathematica.ast._
import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaEvaluator private (globalEnvironment: mutable.Map[String, ASTNode])
  extends LazyLogging {

  /* --- constructors --- */

  def this(startEnvironment: Map[String, ASTNode]=Map.empty) = {
    this(mutable.Map[String, ASTNode](startEnvironment.toSeq:_*))
  }

  /* --- publics --- */

  def eval(node: ASTNode): ASTNode = {
    logger.debug("Evalutating " + node)
    eval(node, globalEnvironment)
  }

  /* --- privates --- */

  private def eval(node: ASTNode, environment: mutable.Map[String, ASTNode]): ASTNode = {
    node match {
      case a: TerminalNode => evalTerminals(a, environment)
      case a @ (PlusNode(_) | TimesNode(_) | DivideNode(_)) => evalMath(a, environment)
      case other: ASTNode => throw new MatchError(other)
    }
  }

  private def evalTerminals(node: TerminalNode, environment: mutable.Map[String, ASTNode]): ASTNode = {
    node match {
      case NumberNode(fraction) if fraction.contains("/") =>
        NumberNode(fraction.split("/").map(BigDecimal(_)).reduce(_ / _).toString)
      case SymbolNode(symbol) => environment(symbol)
      case node: TerminalNode => node
    }
  }

  private def evalMath(node: ASTNode, environment: mutable.Map[String, ASTNode]): NumberNode = {
    node match {
      case PlusNode(arguments) =>
        NumberNode(arguments.map(eval(_, environment).asInstanceOf[NumberNode]).map(s=>BigDecimal(s.value)).sum.toString)
      case TimesNode(arguments) =>
        NumberNode(arguments.map(eval(_, environment).asInstanceOf[NumberNode]).map(s=>BigDecimal(s.value)).product.toString)
      case DivideNode(Seq(first, second)) =>
        (eval(first), eval(second)) match {
          case (NumberNode(a) , NumberNode(b)) => NumberNode((BigDecimal(a) / BigDecimal(b)).toString)
          case other: Any => throw new MatchError(other)
        }
      case other: Any => throw new MatchError(other)
    }
  }
}

