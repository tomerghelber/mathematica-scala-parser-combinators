package tomerghelber.mathematica.parser

/**
 * @author user
 * @since 31-Oct-19
 */
object Delimiters {

  val ROUND_BRACKET_OPEN = "("
  val ROUND_BRACKET_CLOSE = ")"
  val SQUARE_BRACKET_OPEN = "["
  val SQUARE_BRACKET_CLOSE = "]"
  val SEMICOLON = ";"
  val COLON = ":"
  val EXCLAMATION_MARK = "!"
  val EQUALS_SIGN = "="
  val CARET = "^"
  val PLUS = "+"
  val MINUS = "-"
  val PLUS_MINUS = "±"
  val MINUS_PLUS = "∓"
  val ASTERISK = "*"
  val MULTIPLICATION_SIGN = "×"
  val DIVIDE = "/"
  val OBELUS = "÷"
  val INCREASE = "++"
  val DECREASE = "--"

  def values: Set[String] = Set(
    ROUND_BRACKET_OPEN,
    ROUND_BRACKET_CLOSE,
    SQUARE_BRACKET_OPEN,
    SQUARE_BRACKET_CLOSE,
    SEMICOLON,
    COLON,
    EXCLAMATION_MARK,
    EQUALS_SIGN,
    CARET,
    PLUS,
    MINUS,
    PLUS_MINUS,
    MINUS_PLUS,
    ASTERISK,
    MULTIPLICATION_SIGN,
    DIVIDE,
    OBELUS,
    INCREASE,
    DECREASE,
  )
}
