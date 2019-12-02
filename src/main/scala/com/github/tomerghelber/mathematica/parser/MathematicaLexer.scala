package com.github.tomerghelber.mathematica.parser

import scala.collection.mutable
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

/** Lexer for Mathematica tokens.
 */
class MathematicaLexer extends Lexical with StdTokens {

  // see `token` in `Scanners`
  override def token: Parser[Token] =
      ( identifier
      | number
      | string
      | delim
      | failure("illegal character")
      )

  private def number: Parser[NumericLit] = {
    val sign: Parser[String] = acceptMatch("sign", {case '+' => "+" case '-' => "-"})
    val unsignedInteger: Parser[String] = rep1(digit) ^^ { number => number.mkString }
    val signedInteger: Parser[String] = opt(sign) ~ unsignedInteger ^^ {
      case signOptional ~ number => (signOptional ++ number).mkString
    }
    def concat(before: Parser[String], char: Char, after: Parser[String]): Parser[String] =
      before ~ char ~ after ^^ {case p ~ middle ~ q => p + middle + q}
    val signedFraction: Parser[String] = concat(signedInteger, '/', signedInteger)
    val signedFloat: Parser[String] = concat(signedInteger, '.', unsignedInteger)
    val scientificNotation: Parser[String] = concat(signedFloat , 'E', signedInteger)
    (scientificNotation | signedFloat | signedFraction | signedInteger) ^^ NumericLit
  }
  private def identifier = letter ~ rep(letter | digit) ^^ {
    case first ~ lasts =>
      val name = (first :: lasts).mkString
      if (reserved contains name) Keyword(name) else Identifier(name)
  }
  private def string =
    ( '\'' ~> rep( chrExcept('\'', '\n', EofCh) ) <~ '\'' ^^ (chars => StringLit(chars.mkString))
    | '\"' ~> rep( chrExcept('\"', '\n', EofCh) ) <~ '\"' ^^ (chars => StringLit(chars.mkString))
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")
    | '\"' ~> failure("unclosed string literal")
    )

  // see `whitespace in `Scanners`
  override def whitespace: Parser[Any] = rep(whitespaceChar)

  /** The set of reserved identifiers: these will be returned as `Keyword`s. */
  val reserved = new mutable.HashSet[String]

  /** The set of delimiters (ordering does not matter). */
  val delimiters = new mutable.HashSet[String]

  /** Construct parser for delimiters by |'ing together the parsers for the individual delimiters,
   *  starting with the longest one -- otherwise a delimiter D will never be matched if there is
   *  another delimiter that is a prefix of D.
   */
  private lazy val _delim: Parser[Token] = {
    def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }

    val d = new Array[String](delimiters.size)
    delimiters.copyToArray(d, 0)
    scala.util.Sorting.quickSort(d)
    (d.toList map parseDelim).foldRight(failure("no matching delimiter"): Parser[Token])((x, y) => y | x)
  }
  protected def delim: Parser[Token] = _delim
}
