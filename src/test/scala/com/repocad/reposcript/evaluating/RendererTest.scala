package com.repocad.reposcript.evaluating

import com.repocad.remote.HttpClient
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.model.FontMetrics
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{Evaluator, EvaluatorEnv, Renderer}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

class RendererTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter: Renderer = mock[Renderer]
  val env: EvaluatorEnv = EvaluatorEnv()
    .add("line", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", NumberType)),
      UnitType, (funEnv: EvaluatorEnv, a: Double, b: Double, c: Double, d: Double) => mockPrinter.line(a, b, c, d))
  val mockParser = new Parser(mock[HttpClient], ParserEnv(), TokenLexer.lex)
  val mockFontMetrics = mock[FontMetrics]

  "A evaluator" should "evaluate a line call" in {
    (mockPrinter.line _).expects(1.0, 2.0, 3.0, 4.0)
    Evaluator.eval(CallExpr("line", UnitType,
      Seq(NumberExpr(1d), NumberExpr(2), NumberExpr(3d), NumberExpr(4d))), mockParser,
      mockPrinter, mockFontMetrics)
  }
  // Test int -> double conversion

}
