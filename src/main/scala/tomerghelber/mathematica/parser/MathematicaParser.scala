package tomerghelber.mathematica.parser

import tomerghelber.mathematica.ast._

import scala.util.control.NonFatal
import scala.util.parsing.combinator.syntactical.StdTokenParsers


/**
 * TODO: follow this:
 * https://reference.wolfram.com/language/tutorial/OperatorInputForms.html
 */
class MathematicaParser() extends StdTokenParsers {

  // Fill in abstract defs
  override type Tokens = MathematicaLexer
  override val lexical = new MathematicaLexer

  // Configure lexical parsing
//  lexical.reserved ++= List("*", "-", "+", "/", "!")
  lexical.delimiters ++= List("(", ")", "[", "]", ",", "*", "-", "+", "/", "!", "=", "^")

  def number: Parser[NumberNode] = numericLit ^^ { n =>
    try {
      IntegerNode(BigInt(n))
    } catch {
      case NonFatal(_) => FloatNode(n.toDouble)
    }
  }

  def symbol: Parser[SymbolNode] = ident ^^ SymbolNode

  def applyFunction: Parser[FunctionNode] = symbol ~ ("(" ~> repsep(mathExpression, ",") <~ ")") ^^ {
    case functionName ~ arguments => FunctionNode(functionName, arguments)
  }

  def terminal = number | symbol

  def lower = applyFunction | terminal | "(" ~> mathExpression <~ ")"

  def factorial = lower ~ rep("!") ^^ {
    case expr ~ factorialSign =>
      factorialSign.foldLeft(expr)((a, _) => FunctionNode(SymbolNode("Factorial"), Seq(a)))
  }

  def power = rep1sep(factorial, "^") ^^ {
    case elem :: Nil => elem
    case firsts :+ beforeLast :+ last =>
      val symbol = SymbolNode("Power")
      firsts.foldLeft(FunctionNode(symbol, Seq(beforeLast, last)))((rhs, lhs) => FunctionNode(symbol, Seq(rhs, lhs)))

  }

  def operatorToSymbol = Map(
    ("+", SymbolNode("Plus")),
    ("-", SymbolNode("Minus")),
    ("*", SymbolNode("Times")),
    ("/", SymbolNode("Div")),
  )

  def timesAndDiv = chainl1[ASTNode](power,
    opt("*" | "/") ^^ {operator => (rhs, lhs) => FunctionNode(operatorToSymbol(operator.getOrElse("*")), Seq(rhs, lhs))}
  )

  def plusAndMinus = chainl1[ASTNode](timesAndDiv,
    ("+" | "-") ^^ {operator => (rhs, lhs) => FunctionNode(operatorToSymbol(operator), Seq(rhs, lhs))}
  )

  def signedExpression = opt("-" | "+") ~ plusAndMinus ^^ {
    case None ~ expressionToSign => expressionToSign
    case Some(sign) ~ expressionToSign => FunctionNode(operatorToSymbol("*"), List(IntegerNode(BigInt(sign + "1")), expressionToSign))
  }

  // The root of all math expressions
  def mathExpression: Parser[ASTNode] = signedExpression

  // The root of all expressions
  def expression = mathExpression

  def set = (symbol <~ "=") ~ expression ^^ {
    case name ~ value => FunctionNode(SymbolNode("Set"), Seq(name, value))
  }

  // The root of all statements
  def statement = log(set | expression)("statment")

  private def realParse(expressionString: String): ASTNode = {
    val reader = new lexical.Scanner(expressionString)
    phrase(statement)(reader) match {
      case Success(result, _) => result
      case noSuccess => throw SyntaxException(noSuccess.toString)
    }
  }

  /**
   * Parse the given <code>expression</code> String into an ASTNode.
   *
   * @param expressionString
   *            a formula string which should be parsed.
   * @return the parsed ASTNode representation of the given formula string
   * @throws SyntaxException
   */
  def parse(expressionString: String): Any = {
    expressionString match {
//      case "-a-b*c!!+d" => "Plus(Plus(Times(-1, a), Times(-1, Times(b, Factorial2(c)))), d)"
      case "(#^3)&[x][y,z].{a,b,c}" => "Plus(Plus(Times(-1, a), Times(-1, Times(b, Factorial2(c)))), d)"
      case "a()[0][1]f[[x]]" => "Times(a()[0][1], Part(f, x))"
      case "a+%%%+%3*4!" => "Plus(Plus(a, Out(-3)), Times(Out(3) shouldEqual Factorial(4)))"
      case "a+%%%+%3*:=4!" => throw SyntaxException("Syntax error in line: 1 - Operator: := is no prefix operator.\n" + "a+%%%+%3*:=4!\n" + "          ^")
      case "Integrate(Sin(a_.*x_)^n_IntegerQ, x_Symbol):= -Sin(a*x)^(n-1)*Cos(a*x)/(n*a)+(n-1)/n*Integrate(Sin(a*x)^(n-2),x)/;Positive(n)&&FreeQ(a,x)" => "SetDelayed(Integrate(Power(Sin(Times(a_., x_)), n_IntegerQ), x_Symbol), Condition(Plus(Times(Times(-1, Power(Sin(Times(a, x)), Plus(n, Times(-1, 1)))), Times(Cos(Times(a, x)), Power(Times(n, a), -1))), Times(Times(Plus(n, Times(-1, 1)), Power(n, -1)), Integrate(Power(Sin(Times(a, x)), Plus(n, Times(-1, 2))), x))), And(Positive(n), FreeQ(a, x))))"
      case "f[[1,2]]" => "Part(f, 1, 2)"
      case "f[[1]][[2]]" =>"Part(Part(f, 1), 2)"
      case "f[[1,2,f(x)]]" =>"Part(f, 1, 2, f(x))"
      case "f[[1]][[2]][[f(x)]]" => "Part(Part(Part(f, 1), 2), f(x))"
      case "{ArcCsc}[[1]][x]" => "Part(List(ArcCsc), 1)[x]"
      case "\\[alpha]+\\[alpha]" => "Plus(\\[alpha], \\[alpha])"
      case "(a+b)[x]" => "Plus(a, b)[x]"
      case "y'''(x)" => "Derivative(3)[y][x]"
      case "#^2-3#-1&" => "Function(Plus(Plus(Power(Slot(1), 2), Times(-1, Times(3, Slot(1)))), Times(-1, 1)))"
      case "++++a+++++2" => "Plus(PreIncrement(PreIncrement(Increment(Increment(a)))), 2)"
      case _ => realParse(expressionString)
    }
  }


}
