package com.repocad.reposcript

import com.repocad.reposcript.parsing._

/**
  * An environment containing defaults for use in the [[com.repocad.reposcript.parsing.Parser]] and
  * [[Evaluator]].
  */
object Environment {

  type EnvMap = Map[String, (Expr, Any)]

  val primitiveEnv: EnvMap = Map(
    // Calculation primitives
    "+" -> ((FunctionType("+", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberType),
      (env: EvaluatorEnv, a: Double, b: Double) => a + b)),
    "-" -> ((FunctionType("-", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberType),
      (env: EvaluatorEnv, a: Double, b: Double) => a - b)),
    "*" -> ((FunctionType("*", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberType),
      (env: EvaluatorEnv, a: Double, b: Double) => a * b)),
    "/" -> ((FunctionType("/", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberType),
      (env: EvaluatorEnv, a: Double, b: Double) => a / b)),
    "<" -> ((FunctionType("<", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanType),
      (env: EvaluatorEnv, a: Double, b: Double) => a < b)),
    "<=" -> ((FunctionType("<=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanType),
      (env: EvaluatorEnv, a: Double, b: Double) => a <= b)),
    ">" -> ((FunctionType(">", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanType),
      (env: EvaluatorEnv, a: Double, b: Double) => b < a)),
    ">=" -> ((FunctionType(">=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanType),
      (env: EvaluatorEnv, a: Double, b: Double) => b <= a)),
    // Trigonometry
    "cos" -> ((FunctionType("cos", Seq(RefExpr("degrees", NumberType)), NumberType),
      (_: EvaluatorEnv, degrees: Double) => math.cos(degrees))),
    "degrees" -> ((FunctionType("degrees", Seq(RefExpr("degrees", NumberType)), NumberType),
      (_: EvaluatorEnv, degrees: Double) => math.toDegrees(degrees))),
    "sin" -> ((FunctionType("sin", Seq(RefExpr("degrees", NumberType)), NumberType),
      (_: EvaluatorEnv, degrees: Double) => math.sin(degrees))),
    "radians" -> ((FunctionType("radians", Seq(RefExpr("degrees", NumberType)), NumberType),
      (_: EvaluatorEnv, degrees: Double) => math.toRadians(degrees))),
    "tan" -> ((FunctionType("tan", Seq(RefExpr("degrees", NumberType)), NumberType),
      (_: EvaluatorEnv, degrees: Double) => math.tan(degrees))),
    "toInt" -> ((FunctionType("toInt", Seq(RefExpr("number", NumberType)), NumberType),
      (_: EvaluatorEnv, double: Double) => double.toInt))
  )

}
