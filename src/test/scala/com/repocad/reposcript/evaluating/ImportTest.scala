package com.repocad.reposcript.evaluating

import com.repocad.reposcript.parsing._
import com.repocad.reposcript._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

class ImportTest extends FlatSpec with MockFactory with Matchers {

  val mockPrinter : Printer[Any] = mock[Printer[Any]]
  val evaluatorEnv : Env = Map("line" -> ((funEnv : Env, a : Double, b : Double, c : Double, d : Double) => mockPrinter.line(a, b, c, d)))
  val parserEnv : parsing.ValueEnv = Environment.parserValueEnv
  val mockClient = mock[HttpClient]
  val mockParser = new Parser(mockClient, parserEnv, parsing.defaultTypeEnv)
  val evaluator = new Evaluator(mockParser, Map())

  "An import evaluator" should "not evaluate printer calls in imports" in {
    (mockClient.getSynchronous _).expects("get/test1").returning(Response(0, 4, "line(1 2 3 4)"))
    (mockPrinter.line _).expects(1d, 2d, 3d, 4d).never()

    evaluator.eval(ImportExpr("test1"), evaluatorEnv).isRight should equal (true)
  }
  it should "include imported functions in the environment" in {
    (mockClient.getSynchronous _).expects("get/test2").returning(Response(0, 4, "def a(b as Number) = b"))
    val result = evaluator.eval(BlockExpr(Seq(ImportExpr("test2"), RefExpr("a", NumberType))), evaluatorEnv)
    result.isRight should equal(true)
    val fun = result.right.get._2.asInstanceOf[Function2[Env, Double, Double]]
    fun(evaluatorEnv, Math.PI) should equal(Math.PI)
  }
  it should "include definitions in the environment" in {
    (mockClient.getSynchronous _).expects("get/test3").returning(Response(0, 4, "def a = 10"))
    evaluator.eval(BlockExpr(Seq(ImportExpr("test3"), RefExpr("a", NumberType))), evaluatorEnv).isRight should equal (true)
  }

}
