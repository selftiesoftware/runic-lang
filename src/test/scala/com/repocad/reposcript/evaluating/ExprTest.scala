package com.repocad.reposcript.evaluating

import com.repocad.reposcript.HttpClient
import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  * Tests that the evaluator can evaluate [[com.repocad.reposcript.parsing.Expr]]
  */
class ExprTest extends FlatSpec with MockFactory with Matchers {

  val emptyEnv: EvaluatorEnv = EvaluatorEnv()
  val mockParser = new Parser(mock[HttpClient], ParserEnv(), Lexer.lex)
  val evaluator = new Evaluator(mockParser, emptyEnv)

  "An expression evaluator" should "evaluate an empty block expression" in {
    evaluator.eval(BlockExpr(Seq()), emptyEnv) should equal(Right(emptyEnv -> Unit))
  }
  it should "evaluate a non-empty block expression" in {
    evaluator.eval(BlockExpr(Seq(NumberExpr(1))), emptyEnv) should equal(Right(emptyEnv -> 1))
  }
  it should "evaluate a def expression" in {
    evaluator.eval(DefExpr("test", NumberExpr(1)), emptyEnv) should equal(
      Right(EvaluatorEnv().add("test", Nil, NumberType, 1) -> 1))
  }
  it should "evaluate a function expression" in {
    val fun = (env: EvaluatorEnv, a: Int) => a
    val output = evaluator.eval(FunctionType("f", Seq(RefExpr("a", NumberType)), RefExpr("a", NumberType)), emptyEnv).right.get
    output._1.get("f", Seq(RefExpr("a", NumberType)), NumberType) should equal(Some(output._2))
    output._2.asInstanceOf[Function2[EvaluatorEnv, Int, Int]](emptyEnv, 2) should equal(2)
  }

  "An object evaluator" should "evaluate an object expression" in {
    val obj = ObjectType("object", Seq(), AnyType)
    evaluator.eval(obj, emptyEnv) should equal(
      Right(EvaluatorEnv().add("object", Nil, obj, Seq()), Seq()))
  }
  it should "construct an object" in {
    val t = ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType)
    val paramNames = t.params.map(_.name)
    val env = EvaluatorEnv().add("object", t.params, t, paramNames)
    evaluator.eval(CallExpr("object", t, Seq(NumberExpr(12))), env) should equal(
      Right(env, Map("a" -> 12)))
  }
  it should "access an element in an object" in {
    val obj = ObjectType("object", Seq(RefExpr("a", NumberType)), AnyType)
    val params = Map("a" -> 12)
    val env = EvaluatorEnv().add("object", Nil, obj, params)
    evaluator.eval(RefFieldExpr(RefExpr("object", obj), "a", StringType), env) should equal(
      Right(env, 12))
  }
  it should "include default object arguments" in {
    val parent = ObjectType("parent", Seq(RefExpr("a", NumberType)), AnyType)
    val obj = ObjectType("obj", Seq(RefExpr("b", StringType)), parent, Map("a" -> NumberExpr(2)))
    val params = Map("b" -> "hi")
    evaluator.eval(CallExpr("obj", obj, Seq(StringExpr("hi"))),
      EvaluatorEnv()
        .add("parent", parent.params, parent, Seq("a"))
        .add("obj", obj.params, obj, Seq("b")))
      .right.get._2 should equal(Map("a" -> 2, "b" -> "hi"))
  }
  it should "allow overloading objects with the same name" in {
    val obj1 = ObjectType("a", Seq(), AnyType)
    val obj2 = ObjectType("a", Seq(RefExpr("x", NumberType)), AnyType)
    evaluator.eval(BlockExpr(Seq(obj1, obj2)), emptyEnv).right.get._1 should equal(EvaluatorEnv()
      .add("a", Seq(), obj1, Seq())
      .add("a", Seq(RefExpr("x", NumberType)), obj2, Seq("x")))
  }

  "A value evaluator" should "evaluate a boolean expression" in {
    evaluator.eval(BooleanExpr(false), emptyEnv) should equal(Right(emptyEnv -> false))
  }
  it should "evaluate a float expression" in {
    evaluator.eval(NumberExpr(2.2), emptyEnv) should equal(Right(emptyEnv -> 2.2))
  }
  it should "evaluate a int expression" in {
    evaluator.eval(NumberExpr(2), emptyEnv) should equal(Right(emptyEnv -> 2))
  }
  it should "evaluate a string expression" in {
    evaluator.eval(StringExpr("hi"), emptyEnv) should equal(Right(emptyEnv -> "hi"))
  }
  it should "evaluate a unit expression" in {
    evaluator.eval(UnitExpr, emptyEnv) should equal(Right(EvaluatorEnv() -> Unit))
  }

  "A reference evaluator" should "reference a definition" in {
    val number = NumberExpr(3)
    val definition = DefExpr("definition", RefExpr("number", NumberType))
    val reference = RefExpr("definition", NumberType)
    val env = EvaluatorEnv().add("definition", Nil, NumberType, definition)
      .add("number", Nil, NumberType, number)
    evaluator.eval(reference, env) should equal(Right(env, definition))
  }
  it should "call a function through a reference" in {
    val f = FunctionType("f", Seq(), NumberExpr(3))
    val r = RefExpr("f", f)
    val env = EvaluatorEnv()
      .add("f", Nil, f.returnType, (env: EvaluatorEnv) => 3)
    evaluator.eval(CallExpr("f", NumberType, Nil), env) should equal(Right(env -> 3))
  }
  it should "call an object value from a function parameter" in {
    val o = ObjectType("o", Seq(RefExpr("a", NumberType)), AnyType)
    val f = FunctionType("f", Seq(RefExpr("o", o)), RefFieldExpr(RefExpr("o", o), "a", NumberType))
    val env = EvaluatorEnv()
      .add("o", o.params, o, Seq("a"))
      .add("f", f.params, NumberType, (env: EvaluatorEnv, o: Any) => o.asInstanceOf[Map[String, Any]].get("a").get)
    evaluator.eval(CallExpr("f", NumberType, Seq(CallExpr("o", o, Seq(NumberExpr(10))))), env).right.get._2 should equal(10.0)
  }

}