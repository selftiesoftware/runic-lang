package com.repocad.reposcript.evaluating

import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{HttpClient, Environment, Printer}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class PrimitivesTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter : Printer[Any] = mock[Printer[Any]]
  val defaultEnv = Environment.getEvaluatorEnv(mockPrinter)
  val mockParser = new Parser(mock[HttpClient])
  val evaluator = new Evaluator(mockParser)

  def evalPrimitive[T](operand : String, arg1 : Int, arg2 : Int, expected : T) = {
    evaluator.eval(CallExpr(operand, NumberType, Seq(IntExpr(arg1), IntExpr(arg2))), defaultEnv) should equal (Right(defaultEnv -> expected))
    evaluator.eval(CallExpr(operand, NumberType, Seq(FloatExpr(arg1), FloatExpr(arg2))), defaultEnv) should equal (Right(defaultEnv -> expected))
    evaluator.eval(CallExpr(operand, NumberType, Seq(IntExpr(arg1), FloatExpr(arg2))), defaultEnv) should equal (Right(defaultEnv -> expected))
    evaluator.eval(CallExpr(operand, NumberType, Seq(FloatExpr(arg1), FloatExpr(arg2))), defaultEnv) should equal (Right(defaultEnv -> expected))
  }

  "An evaluator for primitive expressions" should "evaluate a plus expression" in {
    evalPrimitive("+", 2, 3, 5)
  }
  it should "evaluate a minus expression" in {
    evalPrimitive("-", 2, 3, -1)
  }
  it should "evaluate a times expression" in {
    evalPrimitive("*", 2, 3, 6)
  }
  it should "evaluate a division expression" in {
    evalPrimitive("/", 3, 2, 1.5)
  }
  it should "evaluate a less-than expression" in {
    evalPrimitive("<", 2, 2, false)
  }
  it should "evaluate a less-than-equals expression" in {
    evalPrimitive("<=", 2, 2, true)
  }
  it should "evaluate a greater-than expression" in {
    evalPrimitive(">", 2, 2, false)
  }
  it should "evaluate a greater-than-equals expression" in {
    evalPrimitive(">=", 2, 2, true)
  }
}
