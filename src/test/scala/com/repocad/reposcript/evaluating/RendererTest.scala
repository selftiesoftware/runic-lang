package com.repocad.reposcript.evaluating

import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{HttpClient, Renderer$}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class RendererTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter: Renderer[Any] = mock[Renderer[Any]]
  val env: EvaluatorEnv = EvaluatorEnv()
    .add("line", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", NumberType)),
      UnitType, (funEnv: EvaluatorEnv, a: Double, b: Double, c: Double, d: Double) => mockPrinter.line(a, b, c, d))
  val mockParser = new Parser(mock[HttpClient], ParserEnv(), Lexer.lex)
  val evaluator = new Evaluator(mockParser, EvaluatorEnv())

  "A evaluator" should "evaluate a line call" in {
    (mockPrinter.line _).expects(1.0, 2.0, 3.0, 4.0)
    evaluator.eval(CallExpr("line", UnitType, Seq(NumberExpr(1d), NumberExpr(2), NumberExpr(3d), NumberExpr(4d))), env)
  }
  // Test int -> double conversion

}
