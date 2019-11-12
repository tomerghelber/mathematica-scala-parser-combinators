package com.github.tomerghelber.mathematica.parser

import com.github.tomerghelber.mathematica.ast
import com.github.tomerghelber.mathematica.ast._
import com.github.tomerghelber.mathematica.parser.Delimiters._
import com.typesafe.scalalogging.LazyLogging

import scala.util.parsing.combinator.syntactical.StdTokenParsers

/** This is a parser of Mathematica language as described at
 * [[https://reference.wolfram.com/language/tutorial/OperatorInputForms.html the official site]].
 *
 * The only important method is [[parse]].
 */
class MathematicaParser extends StdTokenParsers with ParserUtil with LazyLogging {

  // Fill in abstract defs
  override type Tokens = MathematicaLexer
  override val lexical = new MathematicaLexer

  // Configure lexical parsing
//  lexical.reserved ++= List()
  lexical.delimiters ++= Delimiters.values

  private val number: Parser[NumberNode] = numericLit ^^ NumberNode

  private val symbol: Parser[SymbolNode] = ident ^^ SymbolNode

  private val string: Parser[StringNode] = stringLit ^^ StringNode

  private val terminal: Parser[TerminalNode] = number | symbol | string

  private def lower: Parser[ASTNode] = terminal | ROUND_BRACKET_OPEN ~> root <~ ROUND_BRACKET_CLOSE

  private val overAndUnderscript: Parser[ASTNode] = chainr1(lower,
    UNDERSCRIPT ^^ {_=>UnderscriptNode.createBinary}
  | OVERSCRIPT ^^ {_=>OverscriptNode.createBinary}
  )

  private val subscript: Parser[ASTNode] = rep1sep(overAndUnderscript, SUBSCRIPT) ^^
    (subscripts => subscripts.reduceRight(SubscriptNode.apply))

  private val part: Parser[ASTNode] = {
    ((underparts: Parser[ASTNode]) => underparts ~ rep1(
      SQUARE_BRACKET_OPEN  ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE
    ) ^^ {
      case expr ~ parts =>
        FunctionNode(expr, parts.flatten)
    }
    | underparts ~ rep1(
      (SQUARE_BRACKET_OPEN2 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE2)
    | (SQUARE_BRACKET_OPEN3 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE3)
    ) ^^ {
      case expr ~ parts => PartNode(expr +: parts.flatten)
    } | underparts
    )(subscript)
  }

  private val incrementAndDecrement: Parser[ASTNode] = lastFolderRight(part,
    INCREASE ^^ { _=>IncrementNode.createUnary }
  | DECREASE ^^ { _=>DecrementNode.createUnary }
  )

  private val preincrementAndPredecrement: Parser[ASTNode] = firstFolderRight(
    INCREASE ^^ { _=>PreincrementNode.createUnary}
  | DECREASE ^^ { _=>PredecrementNode.createUnary}
    ,
    incrementAndDecrement
  )

  private val composition: Parser[ASTNode] = chainl1(preincrementAndPredecrement,
    COMPOSITION ^^ {_=>CompositionNode.createBinary}
  | RIGHT_COMPOSITION ^^ {_=>RightCompositionNode.createBinary}
  )

//  private def mapAndApply: Parser[ASTNode] = composition ~ ("/@" | "//@" | "@@" | "@@@") ~ composition ^^ {
//    case expr1 ~ "/@" ~ expr2 => MapNode(expr1, expr2)
//    case expr1 ~ "//@" ~ expr2 => MapAllNode(expr1, expr2)
//    case expr1 ~ "@@" ~ expr2 => Apply2Node(expr1, expr2)
//    case expr1 ~ "@@@" ~ expr2 => Apply3Node(expr1, expr2)
//  } | composition

  private val factorial: Parser[ASTNode] = lastFolderRight(composition,
    (EXCLAMATION_MARK ~ EXCLAMATION_MARK) ^^ {_=>Factorial2Node.createUnary}
  ) ~ opt(EXCLAMATION_MARK) ^^ {
    case expr ~ factorialOpt => factorialOpt.map(_=>ast.FactorialNode(expr)).getOrElse(expr)
  }

  private val conjugateAndTranspose: Parser[ASTNode] = lastFolderRight(factorial,
    CONJUGATE ^^ {_=>ConjugateNode.createUnary}
  | TRANSPOSE ^^ {_=>TransposeNode.createUnary}
  | (CONJUGATE_TRANSPOSE | CONJUGATE_TRANSPOSE2) ^^ {_=>ConjugateTransposeNode.createUnary}
  )

  private val derivative: Parser[ASTNode] = conjugateAndTranspose ~ rep("'") ^^ {
    case expr ~ Nil => expr
    case expr ~ derivatives => DerivativeNode(derivatives.size, expr)
  }

//  private def stringJoin: Parser[ASTNode] = derivative ~ ("<>" ~> derivative <~ "<>") ~ derivative ^^ {
//    case expr1 ~ expr2 ~ expr3 => StringJoinNode(expr1, expr2, expr3)
//  } | derivative

  private val power: Parser[ASTNode] = rep1sep(derivative, CARET) ^^ (values => values.reduceRight(PowerNode.apply))

  private val verticalArrowAndVectorOperators: Parser[ASTNode] =
    CURLY_BRACKET_OPEN ~> rep1sep(power, COMMA) <~ CURLY_BRACKET_CLOSE ^^ ListNode.createMany | power

  private val sqrt: Parser[ASTNode] = firstFolderRight(
    SQRT ^^ {_=>SqrtNode.createUnary},
    verticalArrowAndVectorOperators
  )

  private val differentialD: Parser[ASTNode] = firstFolderRight(
    "d" ^^ {_=>DifferentialDNode.createUnary},
    sqrt
  )

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

  private val divide: Parser[ASTNode] = rep1sep(squareAndCircle, DIVIDE | OBELUS | DIVIDE2) ^^
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

  private val plusAndMinus: Parser[ASTNode] = chainr1(times,
    PLUS ^^ {_=> PlusNode.createBinary}
  | MINUS ^^ {_=>(expr1: ASTNode, expr2: ASTNode) => PlusNode(expr1, TimesNode(NumberNode("-1"), expr2))}
  | PLUS_MINUS ^^ {_=> PlusMinusNode.createBinary}
  | MINUS_PLUS ^^ {_=> MinusPlusNode.createBinary}
  )

  private val intersection: Parser[ASTNode] = rep1sep(plusAndMinus, INTERSECTION) ^^ (_.reduce(IntersectionNode.apply))

  private val union: Parser[ASTNode] = rep1sep(intersection, UNION) ^^ (_.reduce(UnionNode.apply))

  private val span: Parser[ASTNode] = union ~ (SPAN ~> union <~ SPAN) ~ union ^^ {
    case i ~ j ~ k => SpanNode(i, j, k)
  } | union

  private val equalities: Parser[ASTNode] = chainl1(span,
     ("==" | "\uF7D9")  ^^ { _ => EqualNode.createBinary        }
   | "!="               ^^ { _ => UnequalNode.createBinary      }
   | ">"                ^^ { _ => GreaterNode.createBinary      }
   | (">=" | "≥" | "⩾") ^^ { _ => GreaterEqualNode.createBinary }
   | "<"                ^^ { _ => LessNode.createBinary         }
   | ("<=" | "≤" | "⩽") ^^ { _ => LessEqualNode.createBinary    }
  )

//  private val horizontalArrowAndVectorOperators: Parser[ASTNode] = equalities
//  private val diagonalArrowOperators: Parser[ASTNode] = horizontalArrowAndVectorOperators

  private val sameQ: Parser[ASTNode] = chainl1(equalities,
    "===" ^^ {_ => SameQNode.createBinary}
   | "=!=" ^^ {_ => UnSameQNode.createBinary}
  )

  private val setRelationOperators: Parser[ASTNode] = chainl1(sameQ,
    "∈" ^^ {_ =>ElementNode.createBinary}
   | "∉" ^^ {_ =>NotElementNode.createBinary}
   | "⊂" ^^ {_ =>SubsetNode.createBinary}
   | "⊃" ^^ {_ =>SupersetNode.createBinary}
  )

//  private def forallAndExists: Parser[ASTNode] = setRelationOperators ~ ("∀" | "∃" | "∄") ~ setRelationOperators ^^ {
//    case expr1 ~ "∀" ~ expr2 => ForAllNode(expr1, expr2)
//    case expr1 ~ "∃" ~ expr2 => ExistsNode(expr1, expr2)
//    case expr1 ~ "∄" ~ expr2 => NotExistsNode(expr1, expr2)
//  } | setRelationOperators
//
  private val not: Parser[ASTNode] = firstFolderRight(("!" | "¬")^^{_=>NotNode.createUnary},
    setRelationOperators
  )

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

  private val rules = chainl1(not,
    ("->" | "\uF522") ^^ {_=>RuleNode.createBinary}
  | (":>" | "\uF51F") ^^ {_=>RuleDelayedNode.createBinary}
  )

  private def root = rules

  /**
   * Parse the given <code>expression</code> String into an ASTNode.
   *
   * @param expressionString
   *            a formula string which should be parsed.
   * @return the parsed ASTNode representation of the given formula string
   * @throws SyntaxException Thrown when given expression is failed to par
   */
  def parse(expressionString: String): ASTNode = {
    logger.debug("Parsing " + expressionString)
    val reader = new lexical.Scanner(expressionString)
    phrase(root)(reader) match {
      case Success(result, _) => result
      case noSuccess: NoSuccess => throw SyntaxException(noSuccess.toString)
    }
  }
}
