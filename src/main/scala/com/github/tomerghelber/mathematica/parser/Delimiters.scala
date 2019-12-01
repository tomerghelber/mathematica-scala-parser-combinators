package com.github.tomerghelber.mathematica.parser

/** All delimiters in Mathematica.
 * @author user
 * @since 31-Oct-19
 */
object Delimiters {

  val ROUND_BRACKET_OPEN = "("
  val ROUND_BRACKET_CLOSE = ")"
  val CURLY_BRACKET_OPEN = "{"
  val CURLY_BRACKET_CLOSE = "}"
  val SQUARE_BRACKET_OPEN = "["
  val SQUARE_BRACKET_CLOSE = "]"
  val SQUARE_BRACKET_OPEN2 = "[["
  val SQUARE_BRACKET_CLOSE2 = "]]"
  val SQUARE_BRACKET_OPEN3 = "〚"
  val SQUARE_BRACKET_CLOSE3 = "〛"
  val MAP_SIGN = "/@"
  val MAP_ALL_SIGN = "//@"
  val APPLY_2_SIGN = "@@"
  val APPLY_3_SIGN = "@@@"
  val SPAN = ";;"
  val DOT = "."
  val CIRCLE = "\uF4A0"
  val INTEGRATE = "∫"
  val SEMICOLON = ";"
  val COLON = ":"
  val COMMA = ","
  val EXCLAMATION_MARK = "!"
  val NOT = "¬"
  val EQUALS_SIGN = "="
  val EQUALITY = "=="
  val EQUALITY1 = "\uF7D9"
  val NOT_EQUALITY = "!="
  val GREATER = ">"
  val GREATER_EQUALITY = ">="
  val GREATER_EQUALITY1 = "≥"
  val GREATER_EQUALITY2 = "⩾"
  val SMALLER = "<"
  val SMALLER_EQUALITY = "<="
  val SMALLER_EQUALITY1 = "≤"
  val SMALLER_EQUALITY2 = "⩽"
  val SAME_Q = "==="
  val NOT_SAME_Q = "=!="
  val DISCRETE = "∂"
  val DELTA = "∇"
  val DISCRETE_SHIFT = "\uF4A3"
  val DISCRETE_RATIO = "\uF4A5"
  val DIFFERENCE_DELTA = "\uF4A4"
  val ELEMENT = "∈"
  val NOT_ELEMENT = "∉"
  val SUBSET = "⊂"
  val SUPERSET = "⊃"
  val FORALL = "∀"
  val EXISTS = "∃"
  val NOT_EXISTS = "∄"
  val APOSTROPHE  = "'"
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
  val RULE1 = "->"
  val RULE2 = "\uF522"
  val RULE_DELAYED1 = ":>"
  val RULE_DELAYED2 = "\uF51F"

  /** Holds all the values in this object */
  def values: Set[String] = Set(
    ROUND_BRACKET_OPEN,
    ROUND_BRACKET_CLOSE,
    CURLY_BRACKET_OPEN,
    CURLY_BRACKET_CLOSE,
    SQUARE_BRACKET_OPEN,
    SQUARE_BRACKET_CLOSE,
    SQUARE_BRACKET_OPEN2,
    SQUARE_BRACKET_CLOSE2,
    SQUARE_BRACKET_OPEN3,
    SQUARE_BRACKET_CLOSE3,
    MAP_SIGN,
    MAP_ALL_SIGN,
    APPLY_2_SIGN,
    APPLY_3_SIGN,
    SPAN,
    DOT,
    CIRCLE,
    INTEGRATE,
    SEMICOLON,
    COLON,
    COMMA,
    EXCLAMATION_MARK,
    NOT,
    EQUALS_SIGN,
    EQUALITY,
    EQUALITY1,
    NOT_EQUALITY,
    GREATER,
    GREATER_EQUALITY,
    GREATER_EQUALITY1,
    GREATER_EQUALITY2,
    SMALLER,
    SMALLER_EQUALITY,
    SMALLER_EQUALITY1,
    SMALLER_EQUALITY2,
    SAME_Q,
    NOT_SAME_Q,
    DISCRETE,
    DELTA,
    DISCRETE_SHIFT,
    DISCRETE_RATIO,
    DIFFERENCE_DELTA,
    ELEMENT,
    NOT_ELEMENT,
    SUBSET,
    SUPERSET,
    FORALL,
    EXISTS,
    NOT_EXISTS,
    APOSTROPHE,
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
    RULE1,
    RULE2,
    RULE_DELAYED1,
    RULE_DELAYED2,
  )
}
