package com.repocad.reposcript.evaluating

import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

import com.repocad.reposcript.{parsing, HttpClient, Printer}

class PrinterTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter : Printer[Any] = mock[Printer[Any]]
  val env : Env = Map("line" -> ((funEnv : Env, a : Double, b : Double, c : Double, d : Double) => mockPrinter.line(a, b, c, d)))
  val mockParser = new Parser(mock[HttpClient], Map(), parsing.emptyTypeEnv)
  val evaluator = new Evaluator(mockParser)

  "A evaluator" should "evaluate a line call" in {
    (mockPrinter.line _).expects(1.0, 2.0, 3.0, 4.0)
    evaluator.eval(CallExpr("line", UnitType, Seq(FloatExpr(1d), FloatExpr(2d), FloatExpr(3d), FloatExpr(4d))), env)
  }
  // Test int -> double conversion

}
