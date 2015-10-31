package com.repocad.reposcript.parsing

import com.repocad.reposcript.{Printer, Environment}
import org.scalamock.scalatest.MockFactory

class DrawingTest extends ParsingTest with MockFactory {

  val mockPrinter : Printer[Any] = mock[Printer[Any]]
  val env = ParserEnv()

  "A parser using default drawing environments" should "parse an arc call" in {
    parseString("arc(1 2 3 4 5)", env).right.get._1 should equal (CallExpr("arc", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(4), NumberExpr(5))))
  }
  it should "parse a bezier curve call" in {
    parseString("bezier(1 1 2 2 3 3 4 4)", env).right.get._1 should equal (CallExpr("bezier", UnitType, Seq(NumberExpr(1), NumberExpr(1), NumberExpr(2), NumberExpr(2), NumberExpr(3), NumberExpr(3), NumberExpr(4), NumberExpr(4))))
  }
  it should "parse a circle call" in {
    parseString("circle(1 2 3)", env).right.get._1 should equal (CallExpr("circle", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3))))
  }
  it should "parse a line call" in {
    parseString("line(1 2 3 4)", env).right.get._1 should equal (CallExpr("line", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(4))))
  }
  it should "parse a text call" in {
    parseString("text(1 2 3 \"hello\")", env).right.get._1 should equal (CallExpr("text", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), StringExpr("hello"))))
  }
  it should "parse a text call with a number" in {
    parseString("text(1 2 3 12.3)", env).right.get._1 should equal (CallExpr("text", UnitType, Seq(NumberExpr(1), NumberExpr(2), NumberExpr(3), NumberExpr(12.3))))
  }
  it should "draw a text" in {
    println(parseString("{def a as Number = 2 + 3 \n text(a 0 30 \"test\")").right.get)
  }

}
