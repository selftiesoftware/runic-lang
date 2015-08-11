package com.repocad.reposcript.parsing

class StringParserTest extends ParsingTest {

  "A text parser" should "parse a empty string" in {
    testEqualsAll(Seq(), "")
  }
  it should "parse empty spaces" in {
    testEqualsAll(Seq(), "  \n  ")
  }
  it should "parse comments" in {
    testEqualsAll(Seq(), "#Comment")
  }
  it should "parse comments with newlines" in {
    testEqualsAll(Seq(), "#Comment\n  ")
  }
  it should "fail gracefully on unclosed brackets" in {
    testEqualsAll(Seq(BlockExpr(Seq())), "(")
  }

}
