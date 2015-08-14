package com.repocad.reposcript.parsing

class ValueTest extends ParsingTest {

  "Value parsing" should "parse an Numbereger" in {
    testEquals(NumberExpr(1), "1")
  }
  it should "parse a string" in {
    testEquals(StringExpr("string"), "\"string\"")
  }
  it should "parse a double" in {
    testEquals(NumberExpr(123.42), "123.42")
  }
  it should "parse true to boolean" in {
    testEquals(BooleanExpr(value = true), "true")
  }
  it should "parse false to boolean" in {
    testEquals(BooleanExpr(value = false), "false")
  }

}
