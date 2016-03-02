package com.repocad.reposcript.evaluating

import com.repocad.reposcript._
import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class ImportTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter: Renderer = mock[Renderer]
  val evaluatorEnv: EvaluatorEnv = EvaluatorEnv()
    .add("line", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", NumberType)),
      UnitType, (funEnv: EvaluatorEnv, a: Double, b: Double, c: Double, d: Double) => mockPrinter.line(a, b, c, d))
  val mockClient = mock[HttpClient]
  val mockParser = new Parser(mockClient, Environment.parserEnv, Lexer.lex)
  val evaluator = new Evaluator(mockParser, EvaluatorEnv())

  "An import evaluator" should "not evaluate printer calls in imports" in {
    (mockClient.getSynchronous _).expects("get/test1").returning(Response(0, 4, "line(1 2 3 4)"))
    (mockPrinter.line _).expects(1d, 2d, 3d, 4d).never()

    evaluator.eval(ImportExpr("test1"), evaluatorEnv).isRight should equal(true)
  }
  it should "include imported functions in the environment" in {
    (mockClient.getSynchronous _).expects("get/test2").returning(Response(0, 4, "def a(b as Number) = b"))
    val result = evaluator.eval(ImportExpr("test2"), evaluatorEnv)
    result.isRight should equal(true)
    val fun = result.right.get._2.asInstanceOf[Function2[EvaluatorEnv, Double, Double]]
    fun(evaluatorEnv, Math.PI) should equal(Math.PI)
  }
  it should "include definitions in the environment" in {
    (mockClient.getSynchronous _).expects("get/test3").returning(Response(0, 4, "def a = 10"))
    evaluator.eval(BlockExpr(Seq(ImportExpr("test3"), RefExpr("a", NumberType))), evaluatorEnv).isRight should equal(true)
  }
  it should "stack import definitions" in {
    val mockClient2 = stub[HttpClient]
    val newParser = new Parser(mockClient2, ParserEnv(), Lexer.lex)
    val newEvaluator = new Evaluator(newParser, EvaluatorEnv())
    (mockClient2.getSynchronous _).when("get/test3").returns(Response(0, 4, "def a = 10"))
    (mockClient2.getSynchronous _).when("get/test4").returns(Response(0, 4, "def b = 12"))
    newEvaluator.eval(BlockExpr(Seq(ImportExpr("test3"), ImportExpr("test4"), RefExpr("a", NumberType), RefExpr("b", NumberType))), evaluatorEnv)
      .isRight should equal(true)
  }

}
