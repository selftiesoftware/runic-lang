package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.{LivePlug, LiveNil}

class StringParserTest extends ParsingTest {

  "A text parser" should "parse a empty string" in {
    testEqualsAll(UnitExpr, "")
  }
  it should "parse empty spaces" in {
    testEqualsAll(UnitExpr, "  \n  ")
  }
  it should "parse comments" in {
    testEqualsAll(UnitExpr, "#Comment")
  }
  it should "parse comments with newlines" in {
    testEqualsAll(UnitExpr, "#Comment\n  ")
  }
  it should "fail gracefully on unclosed brackets" in {
    testEqualsAll(UnitExpr, "(")
  }
  it should "reset environments when ending blocks" in {
    parseStringAll("def a = 10", ParserEnv(), spillEnvironment = false) should equal(
      Right(ExprState(DefExpr("a", NumberExpr(10)), ParserEnv())))
  }
  it should "retain the casing in strings" in {
    val string = "Hello World"
    parseString("def A = \"" + string + "\"", ParserEnv()) should equal (
      Right(ExprState(DefExpr("a", StringExpr(string)), ParserEnv()))
    )
  }

}
