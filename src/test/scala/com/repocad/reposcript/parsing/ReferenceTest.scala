package com.repocad.reposcript.parsing

class ReferenceTest extends ParsingTest {

  "Reference parsing" should "reference an existing definition" in {
    parseString("a", ParserEnv("a" -> NumberExpr(1))).right.get.expr should equal(RefExpr("a", NumberType))
  }
  it should "reference an existing function" in {
    parseString("f()", ParserEnv("f" -> FunctionType("f", Seq(), NumberExpr(1)))).right.get.expr should equal(
      CallExpr("f", NumberType, Seq()))
  }
  it should "reference an existing function with one parameter" in {
    parseString("f(2)", ParserEnv("f" -> FunctionType("f", Seq(RefExpr("a", NumberType)), NumberExpr(1)))).right.get.expr should equal(
      CallExpr("f", NumberType, Seq(NumberExpr(2))))
  }
  it should "reference an existing function with two parameters" in {
    val f = FunctionType("f", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType)), RefExpr("a", NumberType))
    parseString("f(2 3)", ParserEnv("f" -> f)).right.get.expr should equal(CallExpr("f", NumberType, Seq(NumberExpr(2), NumberExpr(3))))
  }
//  it should "choose the correct type" in {
//    println(parseStringAll("def h() = {} def h = 10 def x = 10 * h {}"))//.isRight should equal(true)
//  }
  //   TODO: Better error reportings for function and object calls
  //  it should "fail to reference an existing function with different number of parameters" in {
  //    parseString("f()", ParserEnv("f" -> FunctionExpr("f", Seq(RefExpr("a", NumberType)), NumberExpr(1)))) should equal(Left(Error.EXPECTED_PARAMETER_NUMBER("f", 1, 0)))
  //  }
  //  it should "fail when referencing non-existing parameters in the function body" in {
  //    parseString("def a(b as Number) = c") should equal(Left(Error.REFERENCE_NOT_FOUND("c")))
  //  }
  //  it should "fail when giving a wrongly typed argument to a function" in {
  //    parseString("{ def a(b as Number) = b a(\"hi\") }") should equal(Left(Error.TYPE_MISMATCH("NumberType", "StringType", "calling 'a'")))
  //  }
  it should "infer a super type of a typed argument in a function" in {
    val env = ParserEnv("number" -> NumberType)
    val function = FunctionType("a", Seq(RefExpr("b", NumberType)), NumberType)
    parseString("{ def a(b as Number) = 1 a(3) }", env).right.get.expr should equal(
      BlockExpr(Seq(FunctionType("a", Seq(RefExpr("b", NumberType)), NumberExpr(1)),
        CallExpr("a", NumberType, Seq(NumberExpr(3)))))
    )
  }

  "Consecutive function calls" should "parse operations recursively" in {
    parseString("1 + 2 + 3").right.get.expr should equal(
      CallExpr("+", NumberType, Seq(NumberExpr(1),
        CallExpr("+", NumberType, Seq(NumberExpr(2), NumberExpr(3)))))
    )
  }
  it should "parse expressions in parenthesis before others" in {
    parseString("(1 + 2) + 3").right.get.expr should equal(
      CallExpr("+", NumberType, Seq(
        CallExpr("+", NumberType, Seq(NumberExpr(1), NumberExpr(2))),
        NumberExpr(3))
      )
    )
  }
  it should "parse expressions in parenthesis first, even when last" in {
    parseString("1 + (2 + 3)").right.get.expr should equal(
      CallExpr("+", NumberType, Seq(
        NumberExpr(1),
        CallExpr("+", NumberType, Seq(NumberExpr(2), NumberExpr(3))))
      )
    )
  }
  it should "not parse a reference as a function call in a parameter list" in {
    val env = ParserEnv("a" -> NumberExpr(10),
      "f" -> FunctionType("f", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType)), RefExpr("x", NumberType)),
      "+" -> FunctionType("+", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType)), RefExpr("a", NumberType)))
    parseString("f(a (10 + 10))", env).right.get.expr should equal(
      CallExpr("f", NumberType, Seq(RefExpr("a", NumberType), CallExpr("+", NumberType, Seq(NumberExpr(10), NumberExpr(10))))))
  }
  it should "parse two functions as a part of the other" in {
    val env = ParserEnv("+" -> FunctionType("+", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType)), RefExpr("a", NumberType)),
      "cos" -> FunctionType("+", Seq(RefExpr("a", NumberType)), RefExpr("a", NumberType)))
    parseString("cos(1) + 2", env).right.get.expr should equal(
      CallExpr("+", NumberType, Seq(CallExpr("cos", NumberType, Seq(NumberExpr(1))), NumberExpr(2)))
    )
  }
  it should "store a function in a value and call the reference" in {
    parseStringAll("def f() = 10\ndef g = f\ng()").right.get.expr.asInstanceOf[BlockExpr].expr(2) should equal(
      CallExpr("g", NumberType, Seq()))
  }

  "Object referencing" should "call a previously defined object" in {
    val t = ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType)
    parseString("object(12)", ParserEnv("object" -> t)).right.get.expr should equal(
      CallExpr("object", t, Seq(NumberExpr(12))))
  }
  it should "call an object with default parameters" in {
    val parent = ObjectType("parent", Seq(RefExpr("a", NumberType)), AnyType)
    val obj = ObjectType("object", Seq(RefExpr("b", StringType)), parent, Map("a" -> NumberExpr(9)))
    parseString("obj(\"Hola!\")", ParserEnv("parent" -> parent, "obj" -> obj)).right.get.expr should equal(
      CallExpr("obj", obj, Seq(StringExpr("Hola!")))
    )
  }
  it should "call the correct overloaded object" in {
    val obj1 = ObjectType("a", Seq(RefExpr("a", NumberType)), AnyType)
    val obj2 = ObjectType("a", Seq(RefExpr("a", StringType)), AnyType)
    parseString("a(20)", ParserEnv("a" -> obj1, "a" -> obj2)).right.get.expr should equal(
      CallExpr("a", obj1, Seq(NumberExpr(20)))
    )
  }
  it should "reference a field within an object when given as a function parameter" in {
    val obj = ObjectType("a", Seq(RefExpr("x", NumberType)), AnyType)
    parseString("def f(v as a) = v.x", ParserEnv("a" -> obj)).right.get.expr should equal(
      FunctionType("f", Seq(RefExpr("v", obj)), RefFieldExpr(RefExpr("v", obj), "x", NumberType))
    )
  }

}
