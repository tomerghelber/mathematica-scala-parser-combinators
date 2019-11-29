package com.github.tomerghelber.mathematica.parser

import scala.util.parsing.combinator.Parsers

/** Extends `Parsers` for more utility functions.
 * @author user
 * @since 02-Nov-19
 */
trait ParserUtil {
  self: Parsers =>

  /** A parser generator that, roughly, generalises the rep1sep generator so
   *  that `q`, which parses the separator, produces a right-associative
   *  function that combines the elements it separates.
   *
   * @param p a parser that parses the elements.
   * @param q a parser that parses the token(s) separating the elements, yielding a right-associative function that
   *          combines two elements into one.
   * @tparam T The type of the first element.
   * @return The generated parser.
   */
  def chainr1[T](p: => Parser[T], q: => Parser[(T, T) => T]): Parser[T] = chainr1(p, p, q)

  /** A parser generator that, roughly, generalises the `rep1sep` generator
   *  so that `q`, which parses the separator, produces a right-associative
   *  function that combines the elements it separates.
   *
   * @param first a parser that parses the last element.
   * @param p a parser that parses the subsequent elements
   * @param q a parser that parses the token(s) separating the elements,
   *          yielding a right-associative function that combines two elements
   *          into one.
   * @tparam T The type of the first element.
   * @tparam U The type of the separator.
   * @return The generated parser.
   */
  def chainr1[T, U](first: => Parser[T], p: => Parser[U], q: => Parser[(U, T) => T]): Parser[T]
  = rep(p ~ q) ~ first ^^ {
    case xs ~ x => xs.foldRight(x){case (b ~ f, a) => f(b, a)}
  }

  /** Folding the first into the others.
   * @param p The parser of first the element.
   * @param q The parser of the others.
   * @tparam T The type of the first element.
   * @return A parser which fold the first element into an unary function.
   */
  def lastFolderRight[T](p: => Parser[T], q: => Parser[T => T]): Parser[T] = p ~ rep(q) ^^ {
    case first ~ functions => functions.foldRight(first)(_(_))
  }

  /** Folding the last into the others.
   * @param p The parser of the others.
   * @param q The parser of last the element.
   * @tparam T The type of the last element.
   * @return A parser which fold the last element into an unary function.
   */
  def firstFolderRight[T](q: => Parser[T => T], p: => Parser[T]): Parser[T] = rep(q) ~ p ^^ {
    case functions ~ last => functions.foldRight(last)(_(_))
  }
}
