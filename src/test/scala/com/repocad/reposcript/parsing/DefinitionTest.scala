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
    parseString("def a = 10", ParserEnv()) should equal (Right(DefExpr("a", NumberExpr(10)), ParserEnv("a" -> NumberExpr(10))))
  }
  it should "fail when wrong type is specified" in {
    parseString("def a as Unit = 1").isLeft should equal (true)
  }

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
    parseString("def a() = ", ParserEnv()) should equal (Right(function, ParserEnv("a" -> function)))
  }
  it should "accept references to existing parameters in the function body" in {
    val function = FunctionExpr("a", Seq(RefExpr("b", NumberType)), RefExpr("b", NumberType))
    testEquals(function, "def a(b as Number) = b")
  }
  it should "refer to a Number parameter as a Number in an assignment" in {
    val function = FunctionExpr("a", Seq(RefExpr("b", NumberType)), DefExpr("c", RefExpr("b", NumberType)))
    testEquals(function, "def a(b as Number) = def c as Number = b")
  }
  it should "refer to an object parameter" in {
    val obj = ObjectType("o", Seq(), AnyType)
    val function = FunctionExpr("a", Seq(RefExpr("x", obj)), RefExpr("x", obj))
    testEquals(function, "def a(x as o) = x", ParserEnv("o" -> obj))
  }
  it should "refer to an object element in a function" in {
    val obj = ObjectType("o", Seq(RefExpr("a", NumberType)), AnyType)
    val function = FunctionExpr("a", Seq(RefExpr("x", obj)), RefFieldExpr("x", "a", NumberType))
    testEquals(function, "def a(x as o) = x.a", ParserEnv("o" -> obj))
  }

  /* Objects */
  "A object parser" should "create an object and a type" in {
    parseString("def object(a as Any)", ParserEnv("Any" -> AnyType)).right.get._1 should equal(
      ObjectType("object", Seq(RefExpr("a", AnyType)), AnyType))
  }
  it should "store the object in the environment" in {
    parseString("def object(a as Any)", ParserEnv("Any" -> AnyType)).right.get._2 should equal(
      ParserEnv("object" -> ObjectType("object", Seq(RefExpr("a", AnyType)), AnyType), "Any" -> AnyType))
  }
  it should "call a previously defined object" in {
    val t = ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType)
    parseString("object(12)", ParserEnv("object" -> t)).right.get._1 should equal(
      CallExpr("object", t, Seq(NumberExpr(12))))
  }
  it should "fail when calling an object with the wrong parameter type" in {
    parseString("object(\"string\")", ParserEnv("object" -> ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType))).isLeft should equal(true)
  }
  it should "fail when calling an object with the wrong parameter list length" in {
    parseString("object(12, \"string\")", ParserEnv("object" -> ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType))).isLeft should equal(true)
  }
  it should "refer to an object" in {
    val obj = ObjectType("o", Seq(RefExpr("x", NumberType)), AnyType)
    val instance = CallExpr("o", obj, Seq(NumberExpr(10)))
    val expr = parseStringAll("def o(x as Number) \n def i = o(10) \n i.x", ParserEnv("Number" -> NumberType))
    expr.right.get._1 should equal(
      BlockExpr(Seq(obj, DefExpr("i", instance), RefFieldExpr("i", "x", NumberType))))
  }
  it should "reference a field in an object" in {
    val value = "hello"
    val t = ObjectType("object", Seq(RefExpr("name", StringType)), AnyType)
    parseString("instance.name", ParserEnv("object" -> t, "instance" -> CallExpr("object", t, Seq(StringExpr(value))))).right.get._1 should equal(
      RefFieldExpr("instance", "name", StringType)
    )
  }
  it should "fail to access a field that does not exist in an object" in {
    val value = "hello"
    val t = ObjectType("object", Seq(RefExpr("name", StringType)), AnyType)
    parseString("instance.noField", ParserEnv("object" -> t, "instance" -> CallExpr("object", t, Seq(StringExpr(value))))).left.get should equal(
      Error.OBJECT_UNKNOWN_PARAMETER_NAME("object", "noField"))
  }
  it should "reference another object" in {
    val o1 = ObjectType("o1", Seq(), AnyType)
    parseString("def o2(o as o1)", ParserEnv("o1" -> o1)).right.get._1 should equal(ObjectType("o2", Seq(RefExpr("o", o1.t)), AnyType))
  }
//  it should "define an object recursively" in {
//    def o : ObjectType = ObjectType("o", Seq(RefExpr("a", o)), AnyType)
//    parseString("def o(a as o)").right.get._1 should equal(o)
//  }

}
