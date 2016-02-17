package com.repocad.reposcript.parsing

class ParserEnvTest extends ParsingTest {

  "A parser environment" should "store an expression under a type" in {
    ParserEnv("test" -> NumberExpr(3)).toMap should equal(Map("test" -> Map(NumberType -> NumberExpr(3))))
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
  it should "get overloaded functions with different types" in {
    val function1 = FunctionType("f", Seq(), NumberType)
    val function2 = FunctionType("f", Seq(), UnitType)
    val parser = ParserEnv("f" -> function1, "f" -> function2)
    parser.getAsType("f", function2) should equal(Right(function2))
  }
  it should "insert types as types and not expressions" in {
    val typ = UnitType
    ParserEnv("unit" -> typ).toMap should equal(Map("unit" -> Map(typ -> typ)))
  }
  it should "concatenate two environments from left to right" in {
    val env1 = ParserEnv()
    val env2 = ParserEnv("b" -> NumberExpr(10))
    (env1 ++ env2) should equal(env2)
  }
  it should "concatenate two environments from right to left" in {
    val env1 = ParserEnv("b" -> NumberExpr(10))
    val env2 = ParserEnv()
    (env1 ++ env2) should equal(env1)
  }
  it should "return an expression in a get operation" in {
    val env = ParserEnv("a" -> NumberExpr(21))
    env.get("a") should equal(Right(NumberExpr(21)))
  }
  it should "return the referred expression in a reference" in {
    val env = ParserEnv("a" -> NumberExpr(3), "r" -> RefExpr("a", NumberType))
    env.get("r") should equal(Right(RefExpr("a", NumberType)))
  }
  it should "store ambiguous types" in {
    val function1 = FunctionType("a", Seq(), UnitExpr)
    val function2 = FunctionType("a", Seq(RefExpr("b", NumberType)), UnitExpr)
    ParserEnv("a" -> function1, "a" -> function2).getAll("a").size should equal(2)
  }
  it should "fail when getting ambiguous type" in {
    val function1 = FunctionType("a", Seq(), UnitExpr)
    val function2 = FunctionType("a", Seq(RefExpr("b", NumberType)), UnitExpr)
    ParserEnv("a" -> function1, "a" -> function2).get("a").isLeft should equal(true)
  }
  it should "get ambiguous callables from their parameterTypes" in {
    val function1 = FunctionType("a", Seq(RefExpr("b", StringType)), UnitExpr)
    val function2 = FunctionType("a", Seq(RefExpr("b", NumberType)), UnitExpr)
    ParserEnv("a" -> function1, "a" -> function2).getCallableWithParameters("a", Seq(NumberType)) should equal(Right(function2))
  }

}
