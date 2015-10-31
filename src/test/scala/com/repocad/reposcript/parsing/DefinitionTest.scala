package com.repocad.reposcript.parsing

class DefinitionTest extends ParsingTest {

  /* Values */
  "A parser for definitions" should "parse a definition" in {
    testEquals(DefExpr("a", NumberExpr(10)), "def a = 10")
  }
  it should "parse a definition with type information" in {
    testEquals(DefExpr("a", NumberExpr(10)), "def a as Number = 10")
  }
  it should "set the type to a supertype if requested" in {
    testEquals(DefExpr("a", NumberExpr(10)), "def a as Number = 10")
  }
  it should "store a value in the value environment" in {
    parseString("def a = 10", ParserEnv()) should equal (Right(DefExpr("a", NumberExpr(10)), ParserEnv("a", NumberExpr(10))))
  }
  it should "fail when wrong type is specified" in {
    parseString("def a as Unit = 1").isLeft should equal (true)
  }
  it should ""

  /* Functions */
  "A parser for functions" should "parse a function without parameters and body" in {
    testEquals(FunctionExpr("a", Seq(), UnitExpr), "def a() = ")
  }
  it should "parse a function with one parameter and no body" in {
    testEquals(FunctionExpr("a", Seq(RefExpr("b", NumberType)), UnitExpr), "def a(b as Number) = ")
  }
  it should "parse a function without a parameter but with a body" in {
    testEquals(FunctionExpr("a", Seq(), BlockExpr(Seq(DefExpr("b", NumberExpr(10.2))))), "def a() = { def b = 10.2 }")
  }
  it should "parse a function with two parameters and no body" in {
    testEquals(FunctionExpr("a", Seq(RefExpr("b", NumberType), RefExpr("c", NumberType)), UnitExpr), "def a(b as Number c as Number) = ")
  }
  it should "parse a function with three parameters and no body" in {
    testEquals(FunctionExpr("a", Seq(RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", StringType)), UnitExpr), "def a(b as Number c as Number d as String) = ")
  }
  it should "parse a function with four parameters and no body" in {
    testEquals(FunctionExpr("a", Seq(RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", StringType), RefExpr("e", BooleanType)), UnitExpr), "def a(b as Number c as Number d as String e as Boolean) = ")
  }
  it should "parse a function with a prepended parameter" in {
    testEquals(FunctionExpr("a", Seq(RefExpr("b", NumberType)),UnitExpr), "def (b as Number)a = ")
  }
  it should "store a function in the value environment" in {
    val function = FunctionExpr("a", Seq(), UnitExpr)
    parseString("def a() = ", ParserEnv()) should equal (Right(function, ParserEnv("a", function)))
  }
  it should "accept references to existing parameters in the function body" in {
    val function = FunctionExpr("a", Seq(RefExpr("b", NumberType)), RefExpr("b", NumberType))
    testEquals(function, "def a(b as Number) = b")
  }
  it should "refer to an Number parameter as an Number in an assignment" in {
    val function = FunctionExpr("a", Seq(RefExpr("b", NumberType)), DefExpr("c", RefExpr("b", NumberType)))
    testEquals(function, "def a(b as Number) = def c as Number = b")
  }

}
