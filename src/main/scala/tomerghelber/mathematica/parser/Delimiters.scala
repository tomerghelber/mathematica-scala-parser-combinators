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
  val SQUARE_BRACKET_OPEN2 = "[["
  val SQUARE_BRACKET_CLOSE2 = "]]"
  val SQUARE_BRACKET_OPEN3 = "〚"
  val SQUARE_BRACKET_CLOSE3 = "〛"
  val SPAN = ";;"
  val SEMICOLON = ";"
  val COLON = ":"
  val COMMA = ","
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
  val DIVIDE2 = "\\/"
  val OBELUS = "÷"
  val INCREASE = "++"
  val DECREASE = "--"
  val COMPOSITION = "@*"
  val RIGHT_COMPOSITION = "/*"
  val TRANSPOSE = "\uF3C7"
  val CONJUGATE = "\uF3C8"
  val CONJUGATE_TRANSPOSE = "\uF3C9"
  val CONJUGATE_TRANSPOSE2 = "\uF3CE"
  val SQRT = "\\@"
  val INTERSECTION = "⋂"
  val UNION = "⋃"
  val SUBSCRIPT = "\\_"
  val OVERSCRIPT = "\\&"
  val UNDERSCRIPT = "\\+"

  def values: Set[String] = Set(
    ROUND_BRACKET_OPEN,
    ROUND_BRACKET_CLOSE,
    SQUARE_BRACKET_OPEN,
    SQUARE_BRACKET_CLOSE,
    SQUARE_BRACKET_OPEN2,
    SQUARE_BRACKET_CLOSE2,
    SQUARE_BRACKET_OPEN3,
    SQUARE_BRACKET_CLOSE3,
    SPAN,
    SEMICOLON,
    COLON,
    COMMA,
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
    DIVIDE2,
    OBELUS,
    INCREASE,
    DECREASE,
    COMPOSITION,
    RIGHT_COMPOSITION,
    TRANSPOSE,
    CONJUGATE,
    CONJUGATE_TRANSPOSE,
    CONJUGATE_TRANSPOSE2,
    SQRT,
    INTERSECTION,
    UNION,
    SUBSCRIPT,
    OVERSCRIPT,
    UNDERSCRIPT,
  )
}
