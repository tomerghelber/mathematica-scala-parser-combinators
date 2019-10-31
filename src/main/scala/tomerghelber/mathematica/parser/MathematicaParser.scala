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

  private def number: Parser[NumberNode] = numericLit ^^ { n =>
    try {
      IntegerNode(BigInt(n))
    } catch {
      case NonFatal(_) => FloatNode(n.toDouble)
    }
  }

  private def symbol: Parser[SymbolNode] = ident ^^ SymbolNode

  private def terminal = number | symbol

  private def lower: Parser[ASTNode] = terminal | "(" ~> root <~ ")"

//  private def overscriptAndUnderscript: Parser[ASTNode] = lower ~ ("\\+" | "\\&") ~ lower ^^ {
//    case expr1 ~ "\\&" ~ expr2 => OverscriptNode(expr1, expr2)
//    case expr1 ~ "\\+" ~ expr2 => UnderscriptNode(expr1, expr2)
//  } | lower
//
//  private def subscript: Parser[ASTNode] = overscriptAndUnderscript ~ "\\_" ~ overscriptAndUnderscript ^^ {
//    case expr1 ~ "\\+" ~ expr2 => SubscriptNode(expr1, expr2)
//  } | overscriptAndUnderscript
//
//  private def part: Parser[ASTNode] = subscript ~ (("[" ~> rep1sep(subscript, ",") <~ "]") | ("[[" ~> rep1sep(subscript, ",") <~ "]]") | ("〚" ~> rep1sep(subscript, ",") <~ "〛")) ^^ {
//    case expr ~ parts => PartNode(expr, parts)
//  } | subscript
//
//  private def incementAndDecrement: Parser[ASTNode] = part ~ ("++"|"--") ^^ {
//    case expr ~ "++" => IncrementNode(expr)
//    case expr ~ "--" => DecrementNode(expr)
//  } | part
//
//  private def preincementAndPredecrement: Parser[ASTNode] = ("++"|"--") ~ incementAndDecrement ^^ {
//    case "++" ~ expr => PreincrementNode(expr)
//    case "--" ~ expr => PredecrementNode(expr)
//  } | incementAndDecrement
//
//  private def composition: Parser[ASTNode] = preincementAndPredecrement ~ ("@*" | "/*") ~ preincementAndPredecrement ^^ {
//    case expr1 ~ "@*" ~ expr2 => CompositionNode(expr1, expr2)
//    case expr1 ~ "/*" ~ expr2 => RightCompositionNode(expr1, expr2)
//  } | preincementAndPredecrement
//
//  private def mapAndApply: Parser[ASTNode] = composition ~ ("/@" | "//@" | "@@" | "@@@") ~ composition ^^ {
//    case expr1 ~ "/@" ~ expr2 => MapNode(expr1, expr2)
//    case expr1 ~ "//@" ~ expr2 => MapAllNode(expr1, expr2)
//    case expr1 ~ "@@" ~ expr2 => Apply2Node(expr1, expr2)
//    case expr1 ~ "@@@" ~ expr2 => Apply3Node(expr1, expr2)
//  } | composition

  private def factorial: Parser[ASTNode] = lower ~ opt("!" ~ opt("!")) ^^ {
    case expr ~ None => expr
    case expr ~ Some("!" ~ None) => FactorialNode(expr)
    case expr ~ Some("!" ~ Some("!")) => Factorial2Node(expr)
  }

//  private def conjugateAndTranspose: Parser[ASTNode] = factorial ~ ("\uF3C8" | "\uF3C7" | "\uF3C9" | "\uF3CE") ^^ {
//    case expr ~ "\uF3C8" => ConjugateNode(expr)
//    case expr ~ "\uF3C7" => TransposeNode(expr)
//    case expr ~ ("\uF3C9"|"\uF3CE") => ConjugateTransposeNode(expr)
//  } | factorial
//
//  private def derivative: Parser[ASTNode] = conjugateAndTranspose ~ rep("'") ^^ {
//    case expr ~ number => DerivativeNode(number.size, expr)
//  }
//
//  private def stringJoin: Parser[ASTNode] = derivative ~ ("<>" ~> derivative <~ "<>") ~ derivative ^^ {
//    case expr1 ~ expr2 ~ expr3 => StringJoinNode(expr1, expr2, expr3)
//  } | derivative

  private def power: Parser[ASTNode] = rep1sep(factorial, "^") ^^ (values => values.reduceRight(PowerNode))

//  // TODO: create this one
//  private def verticalArrowAndVectorOperators: Parser[ASTNode] = power
//
//  private def sqrt: Parser[ASTNode] = "\\@" ~ verticalArrowAndVectorOperators ^^ {
//    case "\\@" ~ expr => SqrtNode(expr)
//  } | verticalArrowAndVectorOperators
//
//  private def differentialD: Parser[ASTNode] = "d" ~> sqrt ^^ {
//    expr => DifferentialDNode(expr)
//  } | sqrt
//
//  private def discreteOperators: Parser[ASTNode] = ("∂" | "∇" | "\uF4A3" | "\uF4A5" | "\uF4A4") ~ differentialD ~ differentialD ^^ {
//    case "∂" ~ expr1 ~ expr2 => DNode(expr1, expr2)
//    case  "∇" ~ expr1 ~ expr2 => DelNode(expr1, expr2)
//    case "\uF4A3" ~ expr1 ~ expr2 => DiscreteShiftNode(expr1, expr2)
//    case "\uF4A5" ~ expr1 ~ expr2 => DiscreteRatioNode(expr1, expr2)
//    case "\uF4A4" ~ expr1 ~ expr2 => DifferenceDeltaNode(expr1, expr2)
//  } | differentialD
//
//  private def squareAndCircle: Parser[ASTNode] = discreteOperators
//
//  private def cross: Parser[ASTNode] = squareAndCircle ~ ("\uF4A0" ~> squareAndCircle <~ "\uF4A0") ~ squareAndCircle ^^ {
//    case expr1 ~ expr2 ~ expr3 => CrossNode(expr1, expr2 , expr3)
//  } | squareAndCircle
//
//  private def dot: Parser[ASTNode] = cross ~ ("." ~> cross <~ ".") ~ cross ^^ {
//    case expr1 ~ expr2 ~ expr3 => DotNode(expr1, expr2 , expr3)
//  } | cross
//
//  private def signedExpression: Parser[ASTNode] = ("+" | "-" | "±" | "∓") ~ dot ^^ {
//    case "+" ~ expr => expr
//    case "-" ~ expr => TimesNode(IntegerNode(-1), expr)
//    case "±" ~ expr => SinglePlusMinusNode(expr)
//    case "∓" ~ expr => SingleMinusPlusNode(expr)
//  } | dot
//
  private def divide: Parser[ASTNode] = rep1sep(power, ("/" | "÷" | "\\/"))  ^^ {_.reduceLeft(DivideNode)}

  private def times: Parser[ASTNode] = rep1sep(divide, opt("*" | "×")) ^^ (_.reduceRight(TimesNode))
//
//  private def product: Parser[ASTNode] = times
//
//  private def integrate: Parser[ASTNode] = "∫" ~> product ~ product ^^ {
//    case expr1 ~ expr2 => IntegrateNode(expr1, expr2)
//  } | product
//
//  //  private def sumAndLimit: Parser[ASTNode] = "∑" | ("\uF438" | "\uF439" | "\uF43A") ^^ {
//  //    case "\uF438" ~ e3 => LimitNode(e3, e1, e2)
//  //    case "\uF439" ~ e3 => MaxLimitNode(e3, e1, e2)
//  //    case "\uF43A" ~ e3 => MinLimitNode(e3, e1, e2)
//  //  } | integrate

  private def plusAndMinus: Parser[ASTNode] = {
    val operatorToNodeCreator: String => ((ASTNode, ASTNode) => ASTNode) = {
      case "+" => PlusNode
      case "-" => (expr1, expr2) => PlusNode(expr1, TimesNode(IntegerNode(-1), expr2))
      case "±" => PlusMinusNode
      case "∓" => MinusPlusNode
    }
    rep(times ~ (("+" | "-" | "±" | "∓") ^^ operatorToNodeCreator)) ~ times ^^ {
      case tuples ~ last =>
        tuples.foldRight(last){case (n2 ~ creator, n1) => creator(n2, n1)}
    }
  }

//  private def intersection: Parser[ASTNode] = (plusAndMinus <~ "⋂") ~ plusAndMinus ^^ {
//    case expr1 ~ expr2 => IntersectionNode(expr1, expr2)
//  } | plusAndMinus
//
//  private def union: Parser[ASTNode] = (intersection <~ "⋃") ~ intersection ^^ {
//    case expr1 ~ expr2 => UnionNode(expr1, expr2)
//  } | intersection
//
//  private def span: Parser[ASTNode] = union ~ (";;" ~> union <~ ";;") ~ union ^^ {
//    case i ~ j ~ k => SpanNode(i, j, k)
//  } | union
//
//  private def equalities: Parser[ASTNode] = span ~ ("==" | "\uF7D9" | "!=" | ">" | ">=" | "≥" | "⩾" | "<" | "<=" | "≤" | "⩽") ~ span ^^ {
//    case expr1 ~ ("==" | "\uF7D9") ~ expr2 => EqualNode(expr1, expr2)
//    case expr1 ~ "!=" ~ expr2 => UnequalNode(expr1, expr2)
//    case expr1 ~ ">" ~ expr2 => GreaterNode(expr1, expr2)
//    case expr1 ~ (">=" | "≥" | "⩾") ~ expr2 => GreaterEqualNode(expr1, expr2)
//    case expr1 ~ "<" ~ expr2 => LessNode(expr1, expr2)
//    case expr1 ~ ("<=" | "≤" | "⩽") ~ expr2 => LessEqualNode(expr1, expr2)
//  } | span
//
//  // TODO: check those
//  private def horizontalArrowAndVectorOperators: Parser[ASTNode] = equalities
//  private def diagonalArrowOperators: Parser[ASTNode] = horizontalArrowAndVectorOperators
//
//  private def sameQ: Parser[ASTNode] = diagonalArrowOperators ~ ("===" | "=!=") ~ diagonalArrowOperators ^^ {
//    case expr1 ~ "===" ~ expr2 => SameQNode(expr1, expr2)
//    case expr1 ~ "=!=" ~ expr2 => UnSameQNode(expr1, expr2)
//  } | diagonalArrowOperators
//
//  private def setRelationOperators: Parser[ASTNode] = sameQ ~ ("∈" | "∉" | "⊂" | "⊃") ~ sameQ ^^ {
//    case expr1 ~ "∈" ~ expr2 => ElementNode(expr1, expr2)
//    case expr1 ~ "∉" ~ expr2 => NotElementNode(expr1, expr2)
//    case expr1 ~ "⊂" ~ expr2 => SubsetNode(expr1, expr2)
//    case expr1 ~ "⊃" ~ expr2 => SupersetNode(expr1, expr2)
//  } | sameQ
//
//  private def forallAndExists: Parser[ASTNode] = setRelationOperators ~ ("∀" | "∃" | "∄") ~ setRelationOperators ^^ {
//    case expr1 ~ "∀" ~ expr2 => ForAllNode(expr1, expr2)
//    case expr1 ~ "∃" ~ expr2 => ExistsNode(expr1, expr2)
//    case expr1 ~ "∄" ~ expr2 => NotExistsNode(expr1, expr2)
//  } | setRelationOperators
//
//  private def not: Parser[ASTNode] = ("!" | "¬") ~> forallAndExists ^^ {
//    expr => NotNode(expr)
//  } | forallAndExists
//
//  private def and: Parser[ASTNode] = not ~ ("&&" | "∧" | "⊼") ~ not ^^ {
//    case expr1 ~ ("&&" | "∧") ~ expr2 => AndNode(expr1, expr2)
//    case expr1 ~ "⊼" ~ expr2 => NandNode(expr1, expr2)
//  } | not
//
//  private def xor: Parser[ASTNode] = and ~ ("⊻" | "\uF4A2") ~ and ^^ {
//    case expr1 ~ ("⊻" | "∧") ~ expr2 => XorNode(expr1, expr2)
//    case expr1 ~ "\uF4A2" ~ expr2 => XnorNode(expr1, expr2)
//  } | and
//
//  private def or: Parser[ASTNode] = xor ~ ("||" | "∨" | "⊽") ~ xor ^^ {
//    case expr1 ~ ("||" | "∨") ~ expr2 => OrNode(expr1, expr2)
//    case expr1 ~ ("⊽") ~ expr2 => NorNode(expr1, expr2)
//  } | xor
//
//  private def equivalent: Parser[ASTNode] = (or <~ "⧦") ~ or ^^ {
//    case expr1 ~ expr2 => EquivalentNode(expr1, expr2)
//  } | or
//
//  private def implies: Parser[ASTNode] = (equivalent <~ ("\uF523" | "⥰")) ~ equivalent ^^ {
//    case expr1 ~ expr2 => Implies(expr1, expr2)
//  } | equivalent

//  def tees: Parser[ASTNode] = implies ~ ("⊢" | "⊨" | "⊣" | "⫤" | "⊥" | "⊤") ~ implies ^^ {
//    case expr1 ~ "⊢" ~ expr2 => RightTeeNode(expr1, expr2)
//    case expr1 ~ "⊨" ~ expr2 => DoubleRightTeeNode(expr1, expr2)
//    case expr1 ~ "⊣" ~ expr2 => LeftTeeNode(expr1, expr2)
//    case expr1 ~ "⫤" ~ expr2 => DoubleLeftTeeNode(expr1, expr2)
//    case expr1 ~ "⊥" ~ expr2 => UpTeeNode(expr1, expr2)
//    case expr1 ~ "⊤" ~ expr2 => DownTeeNode(expr1, expr2)
//  } | implies

  private def root = plusAndMinus

  /**
   * Parse the given <code>expression</code> String into an ASTNode.
   *
   * @param expressionString
   *            a formula string which should be parsed.
   * @return the parsed ASTNode representation of the given formula string
   * @throws SyntaxException
   */
  def parse(expressionString: String): ASTNode = {
    val reader = new lexical.Scanner(expressionString)
    phrase(root)(reader) match {
      case Success(result, _) => result
      case noSuccess => throw SyntaxException(noSuccess.toString)
    }
  }
}
