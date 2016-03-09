package com.repocad.reposcript.evaluating

import com.repocad.reposcript._
import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest._

class ImportTest extends FlatSpec with MockFactory with Matchers with BeforeAndAfterEach { this: Suite =>

  val mockPrinter: Renderer = mock[Renderer]
  val evaluatorEnv: EvaluatorEnv = EvaluatorEnv()
    .add("line", Seq(RefExpr("a", NumberType), RefExpr("b", NumberType), RefExpr("c", NumberType), RefExpr("d", NumberType)),
      UnitType, (funEnv: EvaluatorEnv, a: Double, b: Double, c: Double, d: Double) => mockPrinter.line(a, b, c, d))

  def fixture = {
    new {

      var mockClient: HttpClient = mock[HttpClient]
      val mockParser = new Parser(mockClient, Environment.parserEnv, Lexer.lex)
      val evaluator = new Evaluator(mockParser, EvaluatorEnv())
    }
  }

  "An import evaluator" should "not evaluate printer calls in imports" in {
    val f = fixture
    (f.mockClient.getSynchronous _).expects("get/test1").returning(Response(0, 4, "line(1 2 3 4)"))
    (mockPrinter.line _).expects(1d, 2d, 3d, 4d).never()
    f.evaluator.eval(ImportExpr("test1"), evaluatorEnv).isRight should equal(true)
  }
  it should "not evaluate text calls in imports" in {
    val f = fixture
    (f.mockClient.getSynchronous _).expects("get/test1").returning(Response(0, 4, "text(0 0 1 \"Hi\")"))
    (mockPrinter.text(_: Double, _: Double, _: Double, _: Any)).expects(0, 0, 1, "Hi").never()
    f.evaluator.eval(ImportExpr("test1"), evaluatorEnv).isRight should equal(true)
  }
  it should "not evaluate text calls with font in imports" in {
    val f = fixture
    (f.mockClient.getSynchronous _).expects("get/test1").returning(Response(0, 4, "text(0 0 1 \"Hi\", \"Arial\")"))
    (mockPrinter.text(_: Double, _: Double, _: Double, _: Any, _: String)).expects(0, 0, 1, "Hi", "Arial").never()
    f.evaluator.eval(ImportExpr("test1"), evaluatorEnv).isRight should equal(true)
  }
  it should "include imported functions in the environment" in {
    val f = fixture
    (f.mockClient.getSynchronous _).expects("get/test2").returning(Response(0, 4, "def a(b as number) = b"))
    val result = f.evaluator.eval(ImportExpr("test2"), evaluatorEnv)
    result.isRight should equal(true)
    val fun = result.right.get._2.asInstanceOf[Function2[EvaluatorEnv, Double, Double]]
    fun(evaluatorEnv, Math.PI) should equal(Math.PI)
  }
  it should "include definitions in the environment" in {
    val f = fixture
    (f.mockClient.getSynchronous _).expects("get/test3").returning(Response(0, 4, "def a = 10"))
    f.evaluator.eval(BlockExpr(Seq(ImportExpr("test3"), RefExpr("a", NumberType))), evaluatorEnv).isRight should equal(true)
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
  it should "stack import definitions with different type signatures" in {
    val mockClient2 = stub[HttpClient]
    val newParser = new Parser(mockClient2, Environment.parserEnv, Lexer.lex(_, toLowerCase = true))
    val newEvaluator = new Evaluator(newParser, Environment.evaluatorEnv)
    (mockClient2.getSynchronous _).when("get/test3").returns(Response(0, 4, "def f(x as Number) = x"))
    (mockClient2.getSynchronous _).when("get/test4").returns(Response(0, 4, "def f(x as String) = x"))
    newParser.parse("import test3 import test4 f(10) f(\"hi\")").isRight should equal(true)
  }

}
