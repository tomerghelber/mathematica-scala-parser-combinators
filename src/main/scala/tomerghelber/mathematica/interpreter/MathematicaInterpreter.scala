package tomerghelber.mathematica.interpreter

import tomerghelber.mathematica.eval.MathematicaEvaluator
import tomerghelber.mathematica.parser.MathematicaParser

/**
 * @author user
 * @since 02-Nov-19
 */
class MathematicaInterpreter(parser: MathematicaParser, evaluator: MathematicaEvaluator) {
  def run(str: String): Any = {
    evaluator.eval(parser.parse(str))
  }
}
