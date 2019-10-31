package tomerghelber.mathematica.parser

import scala.collection.mutable
import scala.util.parsing.combinator.lexical.Lexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

class MathematicaLexer extends Lexical with StdTokens {

  // see `token` in `Scanners`
  override def token: Parser[Token] =
      ( identifier
      | number
      | string
      | delim
      | failure("illegal character")
      )

  private def sign = accept("sign", {case '+' => "+" case '-' => "-"})
  private def signedInteger = opt(sign) ~ rep1(digit) ^^ {
    case signOptional ~ number => (signOptional ++ number).mkString
  }
  private def number: Parser[NumericLit] = signedInteger ~ opt('.' ~> rep1(digit)) ~ opt('E' ~> signedInteger) ^^ {
    case number ~ afterDotOptional ~ exponentOptional =>
      val afterDotString = afterDotOptional.map(afterDot => ('.' :: afterDot).mkString).getOrElse("")
      val exponentString = exponentOptional.map("E" + _).getOrElse("")
      NumericLit(number + afterDotString + exponentString)
  }
  private def identifier = opt('$') ~ rep1(letter) ^^ {
    case dollar ~ letters =>
      val name = (dollar ++ letters).mkString
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

  private lazy val _delim: Parser[Token] = {
    // construct parser for delimiters by |'ing together the parsers for the individual delimiters,
    // starting with the longest one -- otherwise a delimiter D will never be matched if there is
    // another delimiter that is a prefix of D
    def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }

    val d = new Array[String](delimiters.size)
    delimiters.copyToArray(d, 0)
    scala.util.Sorting.quickSort(d)
    (d.toList map parseDelim).foldRight(failure("no matching delimiter"): Parser[Token])((x, y) => y | x)
  }
  protected def delim: Parser[Token] = _delim
}
