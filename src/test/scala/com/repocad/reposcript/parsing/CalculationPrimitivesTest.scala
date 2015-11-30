package com.repocad.reposcript.parsing

import com.repocad.reposcript.Environment

class CalculationPrimitivesTest extends ParsingTest {

  def testCallExpr(input : String, a : Int, b : Int, op : String, typ : AnyType) = {
    parseString(input, Environment.parserEnv).right.get.expr should equal(CallExpr(op, typ, Seq(NumberExpr(a), NumberExpr(b))))
  }

  "A parser using default calculation primitives" should "parse a plus function" in {
    testCallExpr("10 + 10", 10, 10, "+", NumberType)
  }
  it should "parse a minus function" in {
    testCallExpr("10 - 10", 10, 10, "-", NumberType)
  }
  it should "parse a times function" in {
    testCallExpr("10 * 10", 10, 10, "*", NumberType)
  }
  it should "parse a division function" in {
    testCallExpr("10 / 10", 10, 10, "/", NumberType)
  }
  it should "parse a less-than function" in {
    testCallExpr("10 < 10", 10, 10, "<", BooleanType)
  }
  it should "parse a less-than-equals function" in {
    testCallExpr("10 <= 10", 10, 10, "<=", BooleanType)
  }
  it should "parse a greater-than function" in {
    testCallExpr("10 > 10", 10, 10, ">", BooleanType)
  }
  it should "parse a greater-than-equals function" in {
    testCallExpr("10 >= 10", 10, 10, ">=", BooleanType)
  }

}
