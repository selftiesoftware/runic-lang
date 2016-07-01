package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.Position

class ControlStructuresTest extends ParsingTest {

  def testCode(code: String, expected: Expr) = parseString(code, ParserEnv()).right.get.expr should equal(expected)

  def testCode(code: String, expected: ParserError) = parseString(code, ParserEnv()) should equal(Left(expected))

  "Control structure parsing" should "parse an if statement without an else block" in {
    testCode("if (true) 1", IfExpr(BooleanExpr(true), NumberExpr(1), UnitExpr, AnyType))
  }
  it should "parse an if statment with an else block" in {
    testCode("if (false) 1 else 2", IfExpr(BooleanExpr(false), NumberExpr(1), NumberExpr(2), NumberType))
  }
  it should "fail to parse an if statement with a condition that is not boolean" in {
    testCode("if (1) 1 else 2", ParserError.TYPE_MISMATCH(BooleanType.toString, NumberType.toString)(Position.start))
  }
  it should "parse a loop statement with an unnamed loop variable" in {
    testCode("repeat 5",
      LoopExpr(DefExpr("counter", NumberExpr(1)), NumberExpr(5), UnitExpr))
  }
  it should "parse a loop statement with an unnamed loop variable and a fixed range" in {
    testCode("repeat 1 to 5", LoopExpr(DefExpr("counter", NumberExpr(1)), NumberExpr(5), UnitExpr))
  }
  it should "parse a loop statement with named loop variable" in {
    testCode("repeat 5 using counter",
      LoopExpr(DefExpr("counter", NumberExpr(1)), NumberExpr(5), UnitExpr))
  }
  it should "parse a loop statement with named loop variable and a fixed range" in {
    testCode("repeat 2 to 5 using counter",
      LoopExpr(DefExpr("counter", NumberExpr(2)), NumberExpr(5), UnitExpr))
  }
  it should "refer to a statement from the loop definition" in {
    val env = ParserEnv("a" -> RefExpr("b", NumberType), "b" -> NumberExpr(2))
    parseString("repeat a using c", env).right.get.expr should equal(
      LoopExpr(DefExpr("c", NumberExpr(1)), RefExpr("a", NumberType), UnitExpr))
  }

}
