package tomerghelber.mathematica.parser

import org.scalatest.{FunSuite, Matchers}
import tomerghelber.mathematica.ast.{FloatNode, FunctionNode, IntegerNode, SymbolNode}

class MathematicaParserSpec extends FunSuite with Matchers {
  test("-a-b*c!!+d") {
    val p = new MathematicaParser()
    val obj = p.parse("-a-b*c!!+d")
    obj shouldEqual FunctionNode(
      SymbolNode("Plus"),
      List(
        FunctionNode(
          SymbolNode("Minus"),
          List(
            FunctionNode(
              SymbolNode("Times"),
              List(
                IntegerNode(-1),
                SymbolNode("a")
              )
            ),
            FunctionNode(
              SymbolNode("Times"),
              List(
                SymbolNode("b"),
                FunctionNode(
                  SymbolNode("Factorial"),
                  List(
                    FunctionNode(
                      SymbolNode("Factorial"),
                      List(
                        SymbolNode("c"),
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        SymbolNode("d")
      )
    )
    obj.toString shouldEqual "Plus(Plus(Times(-1, a), Times(-1, Times(b, Factorial2(c)))), d)"
  }

  test("(#^3)&[x][y,z].{a,b,c}") {
    val p = new MathematicaParser()
    val obj = p.parse("(#^3)&[x][y,z].{a,b,c}")
    obj.toString shouldEqual "Plus(Plus(Times(-1, a), Times(-1, Times(b, Factorial2(c)))), d)"
  }

  test("Integrate(Sin(x)^2+3*x^4, x)") {
    val p = new MathematicaParser()
    val obj = p.parse("Integrate(Sin(x)^2+3*x^4, x)")
    obj shouldBe FunctionNode(
      SymbolNode("Integrate"),
      List(
        FunctionNode(
          SymbolNode("Plus"),
          List(
            FunctionNode(
              SymbolNode("Power"),
              List(
                FunctionNode(
                  SymbolNode("Sin"),
                  List(SymbolNode("x"))
                ),
                IntegerNode(2)
              )
            ),
            FunctionNode(
              SymbolNode("Times"),
              List(
                IntegerNode(3),
                FunctionNode(
                  SymbolNode("Power"),
                  List(
                    SymbolNode("x"),
                    IntegerNode(4)
                  )
                )
              )
            )
          )
        ),
        SymbolNode("x")
      )
    )
  }

  test("a()[0][1]f[[x]]") {
    val p = new MathematicaParser()
    val obj = p.parse("a()[0][1]f[[x]]")
    obj.toString shouldEqual "Times(a()[0][1], Part(f, x))"
  }

  test("f(y,z) (a+b+c)") {
    val p = new MathematicaParser()
    val obj = p.parse("f(y,z) (a+b+c)")
    obj shouldBe FunctionNode(
      SymbolNode("Times"),
      List(
        FunctionNode(
          SymbolNode("f"),
          List(
            SymbolNode("y"),
            SymbolNode("z")
          )
        ),
        FunctionNode(
          SymbolNode("Plus"),
          List(
            FunctionNode(
              SymbolNode("Plus"),
              List(
                SymbolNode("a"),
                SymbolNode("b")
              )
            ),
            SymbolNode("c")
          )
        )
      )
    )
  }

  test("$a=2") {
    val p = new MathematicaParser()
    val obj = p.parse("$a=2")
    obj shouldBe FunctionNode(SymbolNode("Set"), List(SymbolNode("$a"), IntegerNode(2)))
  }

  test("4.7942553860420304E-1") {
    val p = new MathematicaParser()
    val obj = p.parse("4.7942553860420304E-1")
    obj shouldBe FloatNode(0.47942553860420306)
  }

  test("a+%%%+%3*4!") {
    val p = new MathematicaParser()
    val obj = p.parse("a+%%%+%3*4!")
    obj.toString shouldEqual "Plus(Plus(a, Out(-3)), Times(Out(3) shouldEqual Factorial(4)))"
  }

  test("a+%%%+%3*:=4!") {
    try {
      val p = new MathematicaParser()
      p.parse("a+%%%+%3*:=4!")
      fail("A SyntaxError exception should occur here")
    } catch {
      case e: SyntaxException =>
        "Syntax error in line: 1 - Operator: := is no prefix operator.\n" + "a+%%%+%3*:=4!\n" + "          ^" shouldEqual e.getMessage
    }
  }

  test("-42424242424242424242") {
    val p = new MathematicaParser()
    val obj = p.parse("-42424242424242424242")
    obj shouldBe IntegerNode(BigInt("-42424242424242424242"))
  }

  test("-42424242424242424242.125") {
    val p = new MathematicaParser()
    val obj = p.parse("-42424242424242424242.125")
    obj shouldBe FloatNode(-42424242424242424242.125)
  }

  test("-3/4") {
    val p = new MathematicaParser()
    val obj = p.parse("-3/4")
    obj shouldBe FunctionNode(SymbolNode("Div"), List(IntegerNode(-3), IntegerNode(4)))
  }

  test("-(3/4)") {
    val p = new MathematicaParser()
    val obj = p.parse("-(3/4)")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(IntegerNode(-1), FunctionNode(SymbolNode("Div"), List(IntegerNode(3), IntegerNode(4)))))
  }

  test("-(Pi/4)") {
    val p = new MathematicaParser()
    val obj = p.parse("-(Pi/4)")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(IntegerNode(-1), FunctionNode(SymbolNode("Div"), List(SymbolNode("Pi"), IntegerNode(4)))))
  }

  test("a*b*c*d") {
    val p = new MathematicaParser()
    val obj = p.parse("a*b*c*d")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(SymbolNode("a"), SymbolNode("b"))), SymbolNode("c"))), SymbolNode("d")))
  }

  test("-a-b*c!!+d dependsOn") {
    val p = new MathematicaParser()
    val obj = p.parse("-a-b*c!!+d")
    //      obj.dependsOn("d") should be true
    //      obj.dependsOn("x") should be false
    fail("not implemented yet")
  }

  test("Integrate(Sin(a_.*x_)^n_IntegerQ, x_Symbol):= -Sin(a*x)^(n-1)*Cos(a*x)/(n*a)+(n-1)/n*Integrate(Sin(a*x)^(n-2),x)/;Positive(n)&&FreeQ(a,x)") {
    val p = new MathematicaParser()
    val obj = p.parse("Integrate(Sin(a_.*x_)^n_IntegerQ, x_Symbol):= -Sin(a*x)^(n-1)*Cos(a*x)/(n*a)+(n-1)/n*Integrate(Sin(a*x)^(n-2),x)/;Positive(n)&&FreeQ(a,x)")
    obj.toString shouldEqual "SetDelayed(Integrate(Power(Sin(Times(a_., x_)), n_IntegerQ), x_Symbol), Condition(Plus(Times(Times(-1, Power(Sin(Times(a, x)), Plus(n, Times(-1, 1)))), Times(Cos(Times(a, x)), Power(Times(n, a), -1))), Times(Times(Plus(n, Times(-1, 1)), Power(n, -1)), Integrate(Power(Sin(Times(a, x)), Plus(n, Times(-1, 2))), x))), And(Positive(n), FreeQ(a, x))))"
  }

  test("Partial apply") {
    val p = new MathematicaParser()
    var obj = p.parse("f[[1,2]]")
    obj.toString shouldEqual "Part(f, 1, 2)"
    obj = p.parse("f[[1]][[2]]")
    obj.toString shouldEqual "Part(Part(f, 1), 2)"
    obj = p.parse("f[[1,2,f(x)]]")
    obj.toString shouldEqual "Part(f, 1, 2, f(x))"
    obj = p.parse("f[[1]][[2]][[f(x)]]")
    obj.toString shouldEqual "Part(Part(Part(f, 1), 2), f(x))"
  }

  test("a sin()cos()x()y z") {
    val p = new MathematicaParser()
    val obj = p.parse("a sin()cos()x()y z")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Times"), List(SymbolNode("a"), FunctionNode(SymbolNode("sin"), List()))), FunctionNode(SymbolNode("cos"), List()))), FunctionNode(SymbolNode("x"), List()))), SymbolNode("y"))), SymbolNode("z")))
  }

  test("((1+x) (5+x))") {
    val p = new MathematicaParser()
    val obj = p.parse("((1+x) (5+x))")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(FunctionNode(SymbolNode("Plus"), List(IntegerNode(1), SymbolNode("x"))), FunctionNode(SymbolNode("Plus"), List(IntegerNode(5), SymbolNode("x")))))
  }

  test("1/((1+x) (5+x))") {
    val p = new MathematicaParser()
    val obj = p.parse("1/((1+x) (5+x))")
    obj shouldEqual FunctionNode(
      SymbolNode("Div"),
      List(
        IntegerNode(1),
        FunctionNode(
          SymbolNode("Times"),
          List(
            FunctionNode(
              SymbolNode("Plus"),
              List(
                IntegerNode(1),
                SymbolNode("x")
              )
            ),
            FunctionNode(
              SymbolNode("Plus"),
              List(
                IntegerNode(5),
                SymbolNode("x")
              )
            ),
          )
        )
      )
    )
  }

  test("Mul is the same 2(x^3) 2*(x^3)") {
    val p = new MathematicaParser()
    var obj = p.parse("2(x^3)")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(IntegerNode(2), FunctionNode(SymbolNode("Power"), List(SymbolNode("x"), IntegerNode(3)))))
    obj = p.parse("2*(x^3)")
    obj shouldBe FunctionNode(SymbolNode("Times"), List(IntegerNode(2), FunctionNode(SymbolNode("Power"), List(SymbolNode("x"), IntegerNode(3)))))
  }

  test("1/2(x^3)") {
    val p = new MathematicaParser()
    val obj = p.parse("1/2(x^3)")
    obj shouldEqual FunctionNode(
      SymbolNode("Times"),
      List(
        FunctionNode(
          SymbolNode("Div"),
          List(
            IntegerNode(1),
            IntegerNode(2)
          )
        ),
        FunctionNode(
          SymbolNode("Power"),
          List(
            SymbolNode("x"),
            IntegerNode(3)
          )
        ),

      )
    )
  }

  test("(a+b)^2 (x+y)^3 (u+w)^4") {
    val p = new MathematicaParser()
    val obj = p.parse("(a+b)^2 (x+y)^3 (u+w)^4")
    obj shouldEqual FunctionNode(
      SymbolNode("Times"),
      List(
        FunctionNode(
          SymbolNode("Times"),
          List(
            FunctionNode(
              SymbolNode("Power"),
              List(
                FunctionNode(
                  SymbolNode("Plus"),
                  List(
                    SymbolNode("a"),
                    SymbolNode("b")
                  )
                ),
                IntegerNode(2)
              )
            ),
            FunctionNode(
              SymbolNode("Power"),
              List(
                FunctionNode(
                  SymbolNode("Plus"),
                  List(
                    SymbolNode("x"),
                    SymbolNode("y")
                  )
                ),
                IntegerNode(3)
              )
            )
          )
        ),
        FunctionNode(
          SymbolNode("Power"),
          List(
            FunctionNode(
              SymbolNode("Plus"),
              List(
                SymbolNode("u"),
                SymbolNode("w")
              )
            ),
            IntegerNode(4)
          )
        )
      )
    )
  }

  test("{ArcCsc}[[1]][x]") {
    val p = new MathematicaParser()
    val obj = p.parse("{ArcCsc}[[1]][x]")
    obj.toString shouldEqual "Part(List(ArcCsc), 1)[x]"
  }

  test("\\[alpha]+\\[alpha]") {
    val p = new MathematicaParser()
    val obj = p.parse("\\[alpha]+\\[alpha]")
    obj.toString shouldEqual "Plus(\\[alpha], \\[alpha])"
  }

  test("(a+b)[x]") {
    val p = new MathematicaParser()
    val obj = p.parse("(a+b)[x]")
    obj.toString shouldEqual "Plus(a, b)[x]"
  }

  test("Derive y'''(x)") {
    val p = new MathematicaParser()
    val obj = p.parse("y'''(x)")
    obj.toString shouldEqual "Derivative(3)[y][x]"
  }

  test("Function #^2-3#-1&") {
    val p = new MathematicaParser()
    val obj = p.parse("#^2-3#-1&")
    obj.toString shouldEqual "Function(Plus(Plus(Power(Slot(1), 2), Times(-1, Times(3, Slot(1)))), Times(-1, 1)))"
  }

  test("IncrementPreIncrement ++++a+++++2") {
    val p = new MathematicaParser()
    val obj = p.parse("++++a+++++2")
    obj.toString shouldEqual "Plus(PreIncrement(PreIncrement(Increment(Increment(a)))), 2)"
  }
}
