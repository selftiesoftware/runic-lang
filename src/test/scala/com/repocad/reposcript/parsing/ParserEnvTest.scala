package com.repocad.reposcript.parsing

class ParserEnvTest extends ParsingTest {

  "A parser environment" should "store an expression under a type" in {
    val parser = new ParserEnv(Map("test" -> Map(AnyType -> NumberType)))
    parser.getAsType("test", AnyType) should equal(Some(NumberType))
  }
  it should "get all stored types" in {
    val parser = ParserEnv("Number" -> NumberType)
    parser.getAll("Number").toSeq should equal(Seq(NumberType))
  }
  it should "override a single expression" in {
    val parser = ParserEnv("Number" -> NumberExpr(3))
    parser.+("Number" -> NumberExpr(4)) should equal(ParserEnv("Number" -> NumberExpr(4)))
  }
  it should "override other subtypes upon insertion" in {
    ParserEnv("x" -> NumberExpr(1)).+("x" -> AnyType) should equal(ParserEnv("x" -> AnyType))
  }
  it should "subtract all subtypes of a type" in {
    val t = NumberType
    ParserEnv("x" -> NumberExpr(1)).-("x", AnyType) should equal(ParserEnv())
  }
  it should "get functions with different types" in {
    val function1 = FunctionType("f", Seq(), NumberType)
    val function2 = FunctionType("f", Seq(), UnitType)
    val parser = ParserEnv(Map[String, Map[AnyType, Expr]]("f" -> Map(function1 -> function1, function2 -> function2)))
    parser.getAsType("f", function2) should equal(Some(function2))
  }

}
