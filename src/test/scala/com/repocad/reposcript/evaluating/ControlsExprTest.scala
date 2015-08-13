package com.repocad.reposcript.evaluating

import com.repocad.reposcript.{HttpClient, Printer}
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests that the evaluator can evaluate [[com.repocad.reposcript.parsing.Expr]]
  */
class ControlsExprTest extends FlatSpec with MockFactory with Matchers {

  val emptyEnv : Env = Map[String, Any]()
  val mockPrinter : Printer[Any] = mock[Printer[Any]]
  val mockEnv : Env = Map("line" -> ((funEnv : Env, a : Int, b : Double, c : Double, d : Double) => mockPrinter.line(a, b, c, d)))
  val mockParser = new Parser(mock[HttpClient], Map(), emptyTypeEnv)
  val evaluator = new Evaluator(mockParser)

   "A control expression evaluator" should "evaluate an if statement where the condition is true" in {
     evaluator.eval(IfExpr(BooleanExpr(true), IntExpr(1), UnitExpr, AnyType), emptyEnv) should equal(Right(emptyEnv -> 1))
   }
   it should "evaluate an if statement where the condition is false but the else does not exist" in {
     evaluator.eval(IfExpr(BooleanExpr(false), IntExpr(1), UnitExpr, AnyType), emptyEnv) should equal(Right(emptyEnv -> Unit))
   }
   it should "evaluate an if statement where the condition is false and the else body exists" in {
     evaluator.eval(IfExpr(BooleanExpr(false), IntExpr(1), IntExpr(2), IntType), emptyEnv) should equal(Right(emptyEnv -> 2))
   }
   it should "evaluate a loop expression with a false condition" in {
     evaluator.eval(LoopExpr(DefExpr("a", IntExpr(1)), IntExpr(2), IntExpr(3)), emptyEnv) should equal(Right(emptyEnv -> 3))
   }
   it should "evaluate a loop once" in {
     (mockPrinter.line _).expects(1.0, 3.0, 4.0, 5.0)
     evaluator.eval(
       LoopExpr(DefExpr("a", IntExpr(1)), IntExpr(2),
       CallExpr("line", UnitType, Seq(RefExpr("a", IntType), FloatExpr(3), FloatExpr(4), FloatExpr(5)))), mockEnv
     ) should equal(Right(mockEnv -> ()))
   }
   it should "evaluate a loop twice" in {
     (mockPrinter.line _).expects(1.0, 3.0, 4.0, 5.0)
     (mockPrinter.line _).expects(2.0, 3.0, 4.0, 5.0)
     evaluator.eval(
       LoopExpr(DefExpr("a", IntExpr(1)), IntExpr(3),
         CallExpr("line", UnitType, Seq(RefExpr("a", IntType), FloatExpr(3), FloatExpr(4), FloatExpr(5)))), mockEnv
     ) should equal(Right(mockEnv -> ()))
   }
  // Find out how to do verify (*) using scala mock
//  it should "never run when loop end < loop start" in {
//    (mockPrinter.line _).expects(new FunctionAdapter4[Double, Double, Double, Double, Boolean](_ => Unit)).never()
//    eval(
//      LoopExpr(DefExpr("a", IntExpr(1)), IntExpr(0),
//        CallExpr("line", UnitType, Seq(RefExpr("a", IntType), FloatExpr(3), FloatExpr(4), FloatExpr(5)))), mockEnv
//    ) should equal(Right(mockEnv -> ()))
//  }

 //  "evaluate an import statement" in {
 //
 //  }

   "A value evaluator" should "evaluate a boolean expression" in {
     evaluator.eval(BooleanExpr(false), emptyEnv) should equal(Right(emptyEnv -> false))
   }
   it should "evaluate a float expression" in {
     evaluator.eval(FloatExpr(2.2), emptyEnv) should equal(Right(emptyEnv -> 2.2))
   }
   it should "evaluate a int expression" in {
     evaluator.eval(IntExpr(2), emptyEnv) should equal(Right(emptyEnv -> 2))
   }
   it should "evaluate a string expression" in {
     evaluator.eval(StringExpr("hi"), emptyEnv) should equal(Right(emptyEnv -> "hi"))
   }

   "A call evaluator" should "evaluate a call expression with zero parameters" in {
     val funEnv = Map("f" -> ((env : Env) => 1))
     evaluator.eval(CallExpr("f", IntType, Seq()), funEnv) should equal(Right(funEnv -> 1))
   }
   it should "evaluate a call expression with one parameter" in {
     val funEnv = Map("f" -> ((env : Env, a : Int) => a))
     evaluator.eval(CallExpr("f", IntType, Seq(IntExpr(1))), funEnv) should equal(Right(funEnv -> 1))
   }
  it should "evaluate a call expression with two parameter" in {
    val funEnv = Map("f" -> ((env : Env, a : Int, b : Int) => b))
    evaluator.eval(CallExpr("f", IntType, Seq(IntExpr(1), IntExpr(2))), funEnv) should equal(Right(funEnv -> 2))
  }
  // Test more...

 }