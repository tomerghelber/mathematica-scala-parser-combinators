package tomerghelber.mathematica.parser

import tomerghelber.mathematica.ast._
import tomerghelber.mathematica.parser.Delimiters._

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
//  lexical.reserved ++= List()
  lexical.delimiters ++= Delimiters.values

  private val number: Parser[NumberNode] = numericLit ^^ { n => NumberNode(n.toDouble) }

  private val symbol: Parser[SymbolNode] = ident ^^ SymbolNode

  private val terminal = number | symbol

  private def lower: Parser[ASTNode] = terminal | ROUND_BRACKET_OPEN ~> root <~ ROUND_BRACKET_CLOSE

  private val overAndUnderscript: Parser[ASTNode] = {
    val operators = (UNDERSCRIPT | OVERSCRIPT) ^^ {
      case OVERSCRIPT => OverscriptNode
      case UNDERSCRIPT => UnderscriptNode
      case other => throw new MatchError(other)  // redundant, just to suppress the compile time warning
    }
    rep(lower ~ operators) ~ lower ^^ {
      case reps ~ expr => reps.foldRight(expr){case (lhs ~ op, rhs) => op(lhs, rhs)}
    }
  }

  private val subscript: Parser[ASTNode] = rep1sep(overAndUnderscript, SUBSCRIPT) ^^
    (subscripts => subscripts.reduceRight(SubscriptNode))

  private val part: Parser[ASTNode] = {
    ((underparts: Parser[ASTNode]) => underparts ~ rep(
          (ROUND_BRACKET_OPEN ~> rep1sep(underparts, COMMA) <~ ROUND_BRACKET_CLOSE)
        | (SQUARE_BRACKET_OPEN2 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE2)
        | (SQUARE_BRACKET_OPEN3 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE3)
    ) ^^ {
      case expr ~ parts => parts.flatten.foldLeft(expr)(PartNode)
    })(subscript)
  }

  private val incrementAndDecrement: Parser[ASTNode] = part ~ rep(INCREASE | DECREASE) ^^ {
    case expr ~ operators => operators.map{
      case INCREASE => IncrementNode
      case DECREASE => DecrementNode
      case other => throw new MatchError(other)  // redundant, just to suppress the compile time warning
    }.foldLeft(expr)((e, op) => op(e))
  }

  private val preincrementAndPredecrement: Parser[ASTNode] = rep(INCREASE | DECREASE) ~ incrementAndDecrement ^^ {
    case operators ~ expr => operators.map{
      case INCREASE => PreincrementNode
      case DECREASE => PredecrementNode
      case other => throw new MatchError(other)  // redundant, just to suppress the compile time warning
    }.foldLeft(expr)((e, op) => op(e))
  }

  private val composition: Parser[ASTNode] = chainl1(preincrementAndPredecrement,
    (COMPOSITION | RIGHT_COMPOSITION) ^^ {
      case COMPOSITION => CompositionNode
      case RIGHT_COMPOSITION => RightCompositionNode
      case other => throw new MatchError(other)  // redundant, just to suppress the compile time warning
    }
  )

//  private def mapAndApply: Parser[ASTNode] = composition ~ ("/@" | "//@" | "@@" | "@@@") ~ composition ^^ {
//    case expr1 ~ "/@" ~ expr2 => MapNode(expr1, expr2)
//    case expr1 ~ "//@" ~ expr2 => MapAllNode(expr1, expr2)
//    case expr1 ~ "@@" ~ expr2 => Apply2Node(expr1, expr2)
//    case expr1 ~ "@@@" ~ expr2 => Apply3Node(expr1, expr2)
//  } | composition

  private val factorial: Parser[ASTNode] = composition ~ rep(EXCLAMATION_MARK ~ EXCLAMATION_MARK) ~
    opt(EXCLAMATION_MARK) ^^ {
    case expr ~ factorial2 ~ factorialOpt =>
      val wrapped = factorialOpt.map(_=>FactorialNode(expr)).getOrElse(expr)
      factorial2.foldLeft(wrapped)((e, _)=> Factorial2Node(e))
  }

  private val conjugateAndTranspose: Parser[ASTNode] = factorial ~ rep(
    (CONJUGATE | TRANSPOSE | CONJUGATE_TRANSPOSE | CONJUGATE_TRANSPOSE2) ^^ {
    case CONJUGATE => ConjugateNode
    case TRANSPOSE => TransposeNode
    case CONJUGATE_TRANSPOSE | CONJUGATE_TRANSPOSE2 => ConjugateTransposeNode
    case other => throw new MatchError(other)  // redundant, just to suppress the compile time warning
  }) ^^ {
    case expr ~ operators => operators.foldLeft(expr)((e, op) => op(e))
  }

  private val derivative: Parser[ASTNode] = conjugateAndTranspose ~ rep("'") ^^ {
    case expr ~ Nil => expr
    case expr ~ derivatives => DerivativeNode(derivatives.size, expr)
  }

//  private def stringJoin: Parser[ASTNode] = derivative ~ ("<>" ~> derivative <~ "<>") ~ derivative ^^ {
//    case expr1 ~ expr2 ~ expr3 => StringJoinNode(expr1, expr2, expr3)
//  } | derivative

  private val power: Parser[ASTNode] = rep1sep(derivative, CARET) ^^ (values => values.reduceRight(PowerNode))

  // TODO: create this one
  private val verticalArrowAndVectorOperators: Parser[ASTNode] = power

  private val sqrt: Parser[ASTNode] = rep(SQRT) ~ verticalArrowAndVectorOperators ^^ {
    case sqrts ~ expr => sqrts.foldRight(expr)((_, e) => SqrtNode(e))
  } | verticalArrowAndVectorOperators

  private val differentialD: Parser[ASTNode] = rep("d") ~ sqrt ^^ {
    case diffs ~ expr => diffs.foldLeft(expr)((e, _) => DifferentialDNode(e))
  }

//  private def discreteOperators: Parser[ASTNode] = ("∂" | "∇" | "\uF4A3" | "\uF4A5" | "\uF4A4") ~ differentialD ~ differentialD ^^ {
//    case "∂" ~ expr1 ~ expr2 => DNode(expr1, expr2)
//    case  "∇" ~ expr1 ~ expr2 => DelNode(expr1, expr2)
//    case "\uF4A3" ~ expr1 ~ expr2 => DiscreteShiftNode(expr1, expr2)
//    case "\uF4A5" ~ expr1 ~ expr2 => DiscreteRatioNode(expr1, expr2)
//    case "\uF4A4" ~ expr1 ~ expr2 => DifferenceDeltaNode(expr1, expr2)
//  } | differentialD

  private val squareAndCircle: Parser[ASTNode] = differentialD

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

  private val divide: Parser[ASTNode] = rep1sep(squareAndCircle, (DIVIDE | OBELUS | DIVIDE2)) ^^
    {_.reduceLeft(DivideNode)}

  private val times: Parser[ASTNode] = rep1sep(divide, opt(ASTERISK | MULTIPLICATION_SIGN)) ^^
    (_.reduceRight(TimesNode))

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

  private val plusAndMinus: Parser[ASTNode] = {
    val operatorToNodeCreator: String => ((ASTNode, ASTNode) => ASTNode) = {
      case PLUS => PlusNode
      case MINUS => (expr1, expr2) => PlusNode(expr1, TimesNode(NumberNode(-1), expr2))
      case PLUS_MINUS => PlusMinusNode
      case MINUS_PLUS => MinusPlusNode
    }
    rep(times ~ ((PLUS | MINUS | PLUS_MINUS | MINUS_PLUS) ^^ operatorToNodeCreator)) ~ times ^^ {
      case tuples ~ last =>
        tuples.foldRight(last){case (n2 ~ creator, n1) => creator(n2, n1)}
    }
  }

  private val intersection: Parser[ASTNode] = rep1sep(plusAndMinus, INTERSECTION) ^^ (_.reduce(IntersectionNode))

  private val union: Parser[ASTNode] = rep1sep(intersection, UNION) ^^ (_.reduce(UnionNode))

  private val span: Parser[ASTNode] = union ~ (SPAN ~> union <~ SPAN) ~ union ^^ {
    case i ~ j ~ k => SpanNode(i, j, k)
  } | union

  private def equalities: Parser[ASTNode] = chainl1(span,
    ( ("==" | "\uF7D9") ^^ {_ => EqualNode}
    | "!=" ^^ { _ => UnequalNode}
    | ">" ^^ { _ => GreaterNode}
    | (">=" | "≥" | "⩾") ^^ { _ => GreaterEqualNode}
    | "<" ^^ { _ => LessNode}
    | ("<=" | "≤" | "⩽") ^^ { _ => LessEqualNode}
    )
  )

  // TODO: check those
  private def horizontalArrowAndVectorOperators: Parser[ASTNode] = equalities
  private def diagonalArrowOperators: Parser[ASTNode] = horizontalArrowAndVectorOperators

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

  private def root = diagonalArrowOperators

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
