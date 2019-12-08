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

  private def elemToTernaryOperator[T](p: Parser[ASTNode], sep: Parser[T], op: ApplyTernaryFunctionNode): Parser[FunctionNode] =
    p ~ (sep ~> p <~ sep) ~ p ^^ {case a ~ b ~ c =>op.createTernary(a, b, c)}

  private def elemToOperator[T](p: Parser[T], op: ApplyBinaryFunctionNode): Parser[(ASTNode, ASTNode) => FunctionNode] =
    p ^^ {_=>op.createBinary}

  private def elemToUnaryOperator[T](p: Parser[T], op: ApplyUnaryFunctionNode): Parser[ASTNode => FunctionNode] =
    p ^^ {_=>op.createUnary}

  private def elemsToOperators[T](elem: Parser[T], firstOp: Parser[(T, T) => T], ops: Parser[(T, T) => T]*): Parser[T] =
    elem ~ ops.foldRight(firstOp){case (op1, op2) => op1 | op2} ~ elem ^^ {
      case expr1 ~ op ~ expr2 => op(expr1, expr2)
    }

  private def lower: Parser[ASTNode] = terminal | ROUND_BRACKET_OPEN ~> root <~ ROUND_BRACKET_CLOSE

  private val overAndUnderscript: Parser[ASTNode] = chainr1(lower,
    elemToOperator(UNDERSCRIPT, UnderscriptNode)
  | elemToOperator(OVERSCRIPT, OverscriptNode)
  )

  private val subscript: Parser[ASTNode] = rep1sep(overAndUnderscript, SUBSCRIPT) ^^
    (subscripts => subscripts.reduceRight(SubscriptNode.apply))

  private val part: Parser[ASTNode] = {
    ((underparts: Parser[ASTNode]) => symbol ~ rep1(
      SQUARE_BRACKET_OPEN  ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE
    ) ^^ {
      case expr ~ parts => FunctionNode(expr, parts.flatten)
    }
    | symbol ~ rep1(
      (SQUARE_BRACKET_OPEN2 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE2)
    | (SQUARE_BRACKET_OPEN3 ~> rep1sep(underparts, COMMA) <~ SQUARE_BRACKET_CLOSE3)
    ) ^^ {
      case expr ~ parts => PartNode(expr +: parts.flatten)
    } | underparts
    )(subscript)
  }

  private val incrementAndDecrement: Parser[ASTNode] = lastFolderRight(part,
    elemToUnaryOperator(INCREASE, IncrementNode)
  | elemToUnaryOperator(DECREASE, DecrementNode)
  )

  private val preincrementAndPredecrement: Parser[ASTNode] = firstFolderRight(
    elemToUnaryOperator(INCREASE, PreincrementNode)
  | elemToUnaryOperator(DECREASE, PredecrementNode)
    ,
    incrementAndDecrement
  )

  private val composition: Parser[ASTNode] = chainl1(preincrementAndPredecrement,
    elemToOperator(COMPOSITION, CompositionNode)
  | elemToOperator(RIGHT_COMPOSITION, RightCompositionNode)
  )

  private def mapAndApply: Parser[ASTNode] = elemsToOperators(
    composition,
    elemToOperator(MAP_SIGN, MapNode),
    elemToOperator(MAP_ALL_SIGN, MapAllNode),
    elemToOperator(APPLY_2_SIGN, Apply2Node),
    elemToOperator(APPLY_3_SIGN, Apply3Node)
  ) | composition

  private val factorial: Parser[ASTNode] = lastFolderRight(mapAndApply,
    elemToUnaryOperator(EXCLAMATION_MARK ~ EXCLAMATION_MARK, Factorial2Node)
  ) ~ opt(EXCLAMATION_MARK) ^^ {
    case expr ~ factorialOpt => factorialOpt.map(_=>ast.FactorialNode(expr)).getOrElse(expr)
  }

  private val conjugateAndTranspose: Parser[ASTNode] = lastFolderRight(factorial,
    elemToUnaryOperator(CONJUGATE, ConjugateNode)
  | elemToUnaryOperator(TRANSPOSE, TransposeNode)
  | elemToUnaryOperator(CONJUGATE_TRANSPOSE | CONJUGATE_TRANSPOSE2, ConjugateTransposeNode)
  )

  private val derivative: Parser[ASTNode] = conjugateAndTranspose ~ rep(APOSTROPHE) ^^ {
    case expr ~ Nil => expr
    case expr ~ derivatives => DerivativeNode(derivatives.size, expr)
  }

  private def stringJoin: Parser[ASTNode] = elemToTernaryOperator(derivative, STRING_JOIN, StringJoinNode) | derivative

  private val power: Parser[ASTNode] = rep1sep(stringJoin, CARET) ^^ (values => values.reduceRight(PowerNode.apply))

  private val verticalArrowAndVectorOperators: Parser[ASTNode] =
    CURLY_BRACKET_OPEN ~> rep1sep(power, COMMA) <~ CURLY_BRACKET_CLOSE ^^ ListNode.createMany | power

  private val sqrt: Parser[ASTNode] = firstFolderRight(
    elemToUnaryOperator(SQRT, SqrtNode),
    verticalArrowAndVectorOperators
  )

  private val differentialD: Parser[ASTNode] = firstFolderRight(
    "d" ^^ {_=>DifferentialDNode.createUnary},
    sqrt
  )

  private def discreteOperators: Parser[ASTNode] = (
    elemToOperator(DISCRETE, DNode)
  | elemToOperator(DELTA, DelNode)
  | elemToOperator(DISCRETE_SHIFT, DiscreteShiftNode)
  | elemToOperator(DISCRETE_RATIO, DiscreteRatioNode)
  | elemToOperator(DIFFERENCE_DELTA, DifferenceDeltaNode)
  ) ~ differentialD ~ differentialD ^^ {
    case op ~ expr1 ~ expr2 => op(expr1, expr2)
  } | differentialD

  private val squareAndCircle: Parser[ASTNode] = discreteOperators

  private def cross: Parser[ASTNode] =
    elemToTernaryOperator(squareAndCircle, CIRCLE, CrossNode) | squareAndCircle

  private def dot: Parser[ASTNode] = elemToTernaryOperator(cross, DOT, DotNode) | cross

//  private def signedExpression: Parser[ASTNode] = ("+" | "-" | "±" | "∓") ~ dot ^^ {
//    case "+" ~ expr => expr
//    case "-" ~ expr => TimesNode(IntegerNode(-1), expr)
//    case "±" ~ expr => SinglePlusMinusNode(expr)
//    case "∓" ~ expr => SingleMinusPlusNode(expr)
//  } | dot

  private val divide: Parser[ASTNode] = rep1sep(dot, DIVIDE | OBELUS | DIVIDE2) ^^
    {_.reduceLeft(DivideNode.apply)}

  private val times: Parser[ASTNode] = rep1sep(divide, opt(ASTERISK | MULTIPLICATION_SIGN)) ^^
    (_.reduceRight(TimesNode.apply))

  private def product: Parser[ASTNode] = times

  private def integrate: Parser[ASTNode] = INTEGRATE ~> product ~ product ^^ {
    case expr1 ~ expr2 => IntegrateNode(expr1, expr2)
  } | product

//  private def sumAndLimit: Parser[ASTNode] = "∑" | ("\uF438" | "\uF439" | "\uF43A") ^^ {
//    case "\uF438" ~ e3 => LimitNode(e3, e1, e2)
//    case "\uF439" ~ e3 => MaxLimitNode(e3, e1, e2)
//    case "\uF43A" ~ e3 => MinLimitNode(e3, e1, e2)
//  } | integrate

  private val plusAndMinus: Parser[ASTNode] = chainr1(integrate,
    elemToOperator(PLUS, PlusNode)
  | MINUS ^^ {_=>(expr1: ASTNode, expr2: ASTNode) => PlusNode(expr1, TimesNode(NumberNode("-1"), expr2))}
  | elemToOperator(PLUS_MINUS, PlusMinusNode)
  | elemToOperator(MINUS_PLUS, MinusPlusNode)
  )

  private val intersection: Parser[ASTNode] = rep1sep(plusAndMinus, INTERSECTION) ^^ (_.reduce(IntersectionNode.apply))

  private val union: Parser[ASTNode] = rep1sep(intersection, UNION) ^^ (_.reduce(UnionNode.apply))

  private val span: Parser[ASTNode] = elemToTernaryOperator(union, SPAN, SpanNode) | union

  private val equalities: Parser[ASTNode] = chainl1(span,
     elemToOperator(EQUALITY | EQUALITY1, EqualNode)
   | elemToOperator(NOT_EQUALITY, UnequalNode)
   | elemToOperator(GREATER, GreaterNode)
   | elemToOperator(GREATER_EQUALITY | GREATER_EQUALITY1 | GREATER_EQUALITY2, GreaterEqualNode)
   | elemToOperator(SMALLER, LessNode)
   | elemToOperator(SMALLER_EQUALITY | SMALLER_EQUALITY1 | SMALLER_EQUALITY2, LessEqualNode)
  )

//  private val horizontalArrowAndVectorOperators: Parser[ASTNode] = equalities
//  private val diagonalArrowOperators: Parser[ASTNode] = horizontalArrowAndVectorOperators

  private val sameQ: Parser[ASTNode] = chainl1(equalities,
     elemToOperator(SAME_Q, SameQNode)
   | elemToOperator(NOT_SAME_Q, UnSameQNode)
  )

  private val setRelationOperators: Parser[ASTNode] = chainl1(sameQ,
     elemToOperator(ELEMENT, ElementNode)
   | elemToOperator(NOT_ELEMENT, NotElementNode)
   | elemToOperator(SUBSET, SubsetNode)
   | elemToOperator(SUPERSET, SupersetNode)
  )

  private def forallAndExists: Parser[ASTNode] = elemsToOperators(setRelationOperators,
    elemToOperator(FORALL, ForAllNode),
    elemToOperator(EXISTS, ExistsNode),
    elemToOperator(NOT_EXISTS, NotExistsNode)
  ) | setRelationOperators

  private val not: Parser[ASTNode] = firstFolderRight((EXCLAMATION_MARK | NOT)^^{_=>NotNode.createUnary},
    forallAndExists
  )

//  private def and: Parser[ASTNode] = elemsToOperators(not,
//    elemToOperator("&&" | "∧", AndNode),
//    elemToOperator("⊼", NandNode)
//  ) | not

//  private def xor: Parser[ASTNode] = elemsToOperators(and,
//    elemToOperator("⊻", XorNode),
//    elemToOperator("\uF4A2", XnorNode)
//  ) | and

//  private def or: Parser[ASTNode] = elemsToOperators(xor,
//      elemToOperator("||" | "∨", AndNode),
//      elemToOperator("⊽", NandNode)
//    )

//  private def equivalent: Parser[ASTNode] = elemsToOperators(or,
//    elemToOperator("⧦", EquivalentNode)
//  ) | or

//  private def implies: Parser[ASTNode] = elemsToOperators(equivalent,
//    elemToOperator("\uF523" | "⥰", ImpliesNode)
//  ) | equivalent

//  def tees: Parser[ASTNode] = elemsToOperators(implies,
//    elemToOperator("⊢", RightTeeNode),
//    elemToOperator("⊨", DoubleRightTeeNode),
//    elemToOperator("⊣", LeftTeeNode),
//    elemToOperator("⫤", DoubleLeftTeeNode),
//    elemToOperator("⊥", UpTeeNode),
//    elemToOperator("⊤", DownTeeNode)
//  ) | implies

  private val rules = chainl1(not,
    elemToOperator(RULE1 | RULE2, RuleNode)
  | elemToOperator(RULE_DELAYED1 | RULE_DELAYED2, RuleDelayedNode)
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
