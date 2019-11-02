package tomerghelber.mathematica.parser

import scala.util.parsing.combinator.Parsers

/**
 * @author user
 * @since 02-Nov-19
 */
trait ParserUtil {
  self: Parsers =>

  def chainr1[T](p: => Parser[T], q: => Parser[(T, T) => T]): Parser[T] = chainr1(p, p, q)
  def chainr1[T, U](first: => Parser[T], p: => Parser[U], q: => Parser[(U, T) => T]): Parser[T]
  = rep(p ~ q) ~ first ^^ {
    case xs ~ x => xs.foldRight(x){case (b ~ f, a) => f(b, a)}
  }
}
