package com.repocad.reposcript.parsing

import com.repocad.reposcript.Environment

class ReferenceTest extends ParsingTest {

  "Reference parsing" should "reference an existing definition" in {
    parseString("a", Map("a" -> NumberExpr(1))).right.get._1 should equal(RefExpr("a", NumberType))
  }
  it should "reference an existing function" in {
    parseString("f()", Map("f" -> FunctionExpr("f", Seq(), NumberExpr(1)))).right.get._1 should equal(CallExpr("f", NumberType, Seq()))
  }
  it should "reference an existing function with one parameter" in {
    parseString("f(2)", Map("f" -> FunctionExpr("f", Seq(RefExpr("a", NumberType)), NumberExpr(1)))).right.get._1 should equal(CallExpr("f", NumberType, Seq(NumberExpr(2))))
  }
  it should "fail to reference an existing function with different number of parameters" in {
    parseString("f()", Map("f" -> FunctionExpr("f", Seq(RefExpr("a", NumberType)), NumberExpr(1)))) should equal(Left(Error.EXPECTED_PARAMETER_NUMBER("f", 1, 0)))
  }
  it should "fail when referencing non-existing parameters in the function body" in {
    parseString("def a(b as Number) = c") should equal(Left(Error.REFERENCE_NOT_FOUND("c")))
  }
  it should "fail when giving a wrongly typed argument to a function" in {
    parseString("{ def a(b as Number) = b a(\"hi\") }") should equal(Left(Error.TYPE_MISMATCH("NumberType", "StringType", "calling a")))
  }
  it should "infer a super type of a typed argument in a function" in {
    parseString("{ def a(b as Number) = 1 a(3) }", Map(), defaultTypeEnv) should equal(
      Right(BlockExpr(Seq(FunctionExpr("a", Seq(RefExpr("b", NumberType)), NumberExpr(1)), CallExpr("a", NumberType, Seq(NumberExpr(3))))), Map(), defaultTypeEnv)
    )
  }

}
