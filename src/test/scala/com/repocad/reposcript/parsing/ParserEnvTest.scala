package com.repocad.reposcript.parsing

class ParserEnvTest extends ParsingTest {

  "A parser environment" should "find a pure overloaded type" in {
    val parser = new ParserEnv(Map("test" -> Map(AnyType -> NumberType)))
    parser.getType("test") should equal(Some(NumberType))
  }
  it should "get a stored type" in {
    val parser = ParserEnv("Number" -> NumberType)
    parser.getType("Number") should equal(Some(NumberType))
  }
  it should "override expression with same types" in {
    val parser = ParserEnv("Number" -> NumberExpr(3))
    parser.+("Number" -> NumberExpr(4)) should equal(ParserEnv("Number" -> NumberExpr(4)))
  }

}
