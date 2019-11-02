package tomerghelber.mathematica.parser

import tomerghelber.mathematica.ast._
import tomerghelber.mathematica.parser.Delimiters.{DECREASE, MINUS, MINUS_PLUS, PLUS, PLUS_MINUS, _}

import scala.util.parsing.combinator.syntactical.StdTokenParsers

/** This is a parser of Mathematica language as described at
 * [[https://reference.wolfram.com/language/tutorial/OperatorInputForms.html the official site]].
 *
 * The only important method is [[parse]].
 */
class MathematicaParser extends StdTokenParsers {

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
    val operators =
      ( UNDERSCRIPT ^^ {_=>UnderscriptNode}
      | OVERSCRIPT ^^ {_=>OverscriptNode}
      )
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

  private val incrementAndDecrement: Parser[ASTNode] = part ~ rep(
      INCREASE ^^ { _=>(e1: ASTNode) => IncrementNode.apply(e1) }
    | DECREASE ^^ { _=>(e1: ASTNode) => DecrementNode.apply(e1) }
    ) ^^ { case expr ~ operators => operators.foldRight(expr)(_(_)) }

  private val preincrementAndPredecrement: Parser[ASTNode] =
    rep( INCREASE ^^ { _=>(e1: ASTNode) => PreincrementNode.apply(e1)}
       | DECREASE ^^ { _=>(e1: ASTNode) => PredecrementNode.apply(e1)}
    ) ~ incrementAndDecrement ^^ {
    case operators ~ expr => operators.foldRight(expr)(_(_))
  }

  private val composition: Parser[ASTNode] = chainl1(preincrementAndPredecrement,
    ( COMPOSITION ^^ {_=>CompositionNode}
    | RIGHT_COMPOSITION ^^ {_=>RightCompositionNode}
    )
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
    ( CONJUGATE ^^ {_=>ConjugateNode}
    | TRANSPOSE ^^ {_=>TransposeNode}
    | (CONJUGATE_TRANSPOSE | CONJUGATE_TRANSPOSE2) ^^ {_=>ConjugateTransposeNode}
    )
  ) ^^ { case expr ~ operators => operators.foldRight(expr)(_(_)) }

  private val derivative: Parser[ASTNode] = conjugateAndTranspose ~ rep("'") ^^ {
    case expr ~ Nil => expr
    case expr ~ derivatives => DerivativeNode(derivatives.size, expr)
  }

//  private def stringJoin: Parser[ASTNode] = derivative ~ ("<>" ~> derivative <~ "<>") ~ derivative ^^ {
//    case expr1 ~ expr2 ~ expr3 => StringJoinNode(expr1, expr2, expr3)
//  } | derivative

  private val power: Parser[ASTNode] = rep1sep(derivative, CARET) ^^ (values => values.reduceRight(PowerNode.apply))

  // TODO: create this one
  private val verticalArrowAndVectorOperators: Parser[ASTNode] = power

  private val sqrt: Parser[ASTNode] = rep(SQRT) ~ verticalArrowAndVectorOperators ^^ {
    case sqrts ~ expr => sqrts.foldRight(expr)((_, e) => SqrtNode(e))
  }

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
    {_.reduceLeft(DivideNode.apply)}

  private val times: Parser[ASTNode] = rep1sep(divide, opt(ASTERISK | MULTIPLICATION_SIGN)) ^^
    (_.reduceRight(TimesNode.apply))

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
    val operators: Parser[(ASTNode, ASTNode) => ASTNode] =
    ( PLUS ^^ {_=> (e1: ASTNode, e2: ASTNode) => PlusNode.apply(e1, e2)}
    | MINUS ^^ {_=>(expr1: ASTNode, expr2: ASTNode) => PlusNode(expr1, TimesNode(NumberNode(-1), expr2))}
    | PLUS_MINUS ^^ {_=> (expr1: ASTNode, expr2: ASTNode) => PlusMinusNode(expr1, expr2)}
    | MINUS_PLUS ^^ {_=> (expr1: ASTNode, expr2: ASTNode) => MinusPlusNode(expr1, expr2)}
    )
    rep(times ~ operators) ~ times ^^ {
      case tuples ~ last =>
        tuples.foldRight(last){case (n2 ~ creator, n1) => creator(n2, n1)}
    }
  }

  private val intersection: Parser[ASTNode] = rep1sep(plusAndMinus, INTERSECTION) ^^ (_.reduce(IntersectionNode))

  private val union: Parser[ASTNode] = rep1sep(intersection, UNION) ^^ (_.reduce(UnionNode))

  private val span: Parser[ASTNode] = union ~ (SPAN ~> union <~ SPAN) ~ union ^^ {
    case i ~ j ~ k => SpanNode(i, j, k)
  } | union

  private val equalities: Parser[ASTNode] = chainl1(span,
     ("==" | "\uF7D9") ^^ {_ => EqualNode}
   | "!=" ^^ { _ => UnequalNode}
   | ">" ^^ { _ => GreaterNode}
   | (">=" | "≥" | "⩾") ^^ { _ => GreaterEqualNode}
   | "<" ^^ { _ => LessNode}
   | ("<=" | "≤" | "⩽") ^^ { _ => LessEqualNode}
  )

  // TODO: check those
  private val horizontalArrowAndVectorOperators: Parser[ASTNode] = equalities
  private val diagonalArrowOperators: Parser[ASTNode] = horizontalArrowAndVectorOperators

  private val sameQ: Parser[ASTNode] = chainl1(diagonalArrowOperators,
    "===" ^^ {_ => SameQNode}
   | "=!=" ^^ {_ => UnSameQNode}
  )

  private val setRelationOperators: Parser[ASTNode] = chainl1(sameQ,
    "∈" ^^ {_ => ElementNode}
   | "∉" ^^ {_ => NotElementNode}
   | "⊂" ^^ {_ => SubsetNode}
   | "⊃" ^^ {_ => SupersetNode}
  )

//  private def forallAndExists: Parser[ASTNode] = setRelationOperators ~ ("∀" | "∃" | "∄") ~ setRelationOperators ^^ {
//    case expr1 ~ "∀" ~ expr2 => ForAllNode(expr1, expr2)
//    case expr1 ~ "∃" ~ expr2 => ExistsNode(expr1, expr2)
//    case expr1 ~ "∄" ~ expr2 => NotExistsNode(expr1, expr2)
//  } | setRelationOperators
//
  private val not: Parser[ASTNode] = rep(("!" | "¬")^^{_=>NotNode}) ~ setRelationOperators ^^ {
    case nots ~ expr => nots.foldRight(expr)(_(_))
  }

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

  private def root = not

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
      case noSuccess: NoSuccess => throw SyntaxException(noSuccess.toString)
    }
  }
}
