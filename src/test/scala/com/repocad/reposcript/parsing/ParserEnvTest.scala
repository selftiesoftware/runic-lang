package com.repocad.reposcript.parsing

class ParserEnvTest extends ParsingTest {

  "A parser environment" should "find a pure overloaded type" in {
    val parser = new ParserEnv(Map("test" -> Map(AnyType -> NumberType)))
    parser.getType("test") should equal(NumberType)
  }


}
