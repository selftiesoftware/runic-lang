package com.repocad.reposcript.parsing

import com.repocad.reposcript.{Compiler, Environment, Renderer}
import org.scalamock.scalatest.MockFactory

class DrawingTest extends ParsingTest with MockFactory {

  val mockPrinter: Renderer = mock[Renderer]
  val env = Compiler.defaultEnv

  "A parser using default drawing environments" should "parse an arc call" in {
    parseString("arc(1 2 3 4 5)", env).right.get.expr should equal(CallExpr("arc", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(4), NumberExpr(5))))
  }
  it should "parse a bezier curve call" in {
    parseString("bezier(1 1 2 2 3 3 4 4)", env).right.get.expr should equal(CallExpr("bezier", UnitType, Seq(NumberExpr(1), NumberExpr(1), NumberExpr(2), NumberExpr(2), NumberExpr(3), NumberExpr(3), NumberExpr(4), NumberExpr(4))))
  }
  it should "parse a circle call" in {
    parseString("circle(1 2 3)", env).right.get.expr should equal(CallExpr("circle", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3))))
  }
  it should "parse a line call" in {
    parseString("line(1 2 3 4)", env).right.get.expr should equal(CallExpr("line", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(4))))
  }
  it should "parse a text call" in {
    parseString("text(1 2 3 \"hello\")", env).right.get.expr should equal(CallExpr("text", Compiler.vectorType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), StringExpr("hello"))))
  }
  it should "parse a text call with a number" in {
    parseString("text(1 2 3 12.3)", env).right.get.expr should equal(CallExpr("text", Compiler.vectorType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(12.3))))
  }
  it should "parse a text with a calculated number" in {
    val q = parseString("{def a as Number = 2 + 3 \n text(a 0 30 \"test\")").right.get.expr
    val e = BlockExpr(Seq(DefExpr("a", CallExpr("+", NumberType, Seq(NumberExpr(2), NumberExpr(3)))),
      CallExpr("text", Compiler.vectorType, Seq(RefExpr("a", NumberType), NumberExpr(0), NumberExpr(30), StringExpr("test")))))
    q should equal(e)
  }

}
