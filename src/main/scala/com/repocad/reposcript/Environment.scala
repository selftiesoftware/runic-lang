package com.repocad.reposcript

import com.repocad.reposcript.parsing._

/**
 * An environment containing defaults for use in the [[com.repocad.reposcript.parsing.Parser]] and
 * [[com.repocad.reposcript.evaluating.Evaluator]].
 */
object Environment {

  type EnvMap = Map[String, (Expr, Any)]

  private val BooleanTypeExpr = new Expr { val t = BooleanType }
  private val NumberTypeExpr = new Expr { val t = NumberType }

  private val primitiveEnv : EnvMap = Map (
  // Calculation primitives
    "+" -> (FunctionExpr("+", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env: evaluating.Env, a: Any, b: Any) => RepoMath.plus(a, b)),
    "-" -> (FunctionExpr("-", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.minus(a, b)),
    "*" -> (FunctionExpr("*", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.times(a, b)),
    "/" -> (FunctionExpr("/", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.divide(a, b)),
    "<" -> (FunctionExpr("<", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThan(a, b)),
    "<=" -> (FunctionExpr("<=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThanEquals(a, b)),
    ">" -> (FunctionExpr(">", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThan(b, a)),
    ">=" -> (FunctionExpr(">=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThanEquals(b, a)),
  // Trigonometry
    "cos" -> (FunctionExpr("cos", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.cos(degrees)),
    "degrees" -> (FunctionExpr("degrees", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.toDegrees(degrees)),
    "sin" -> (FunctionExpr("sin", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.sin(degrees)),
    "radians" -> (FunctionExpr("radians", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.toRadians(degrees)),
    "tan" -> (FunctionExpr("tan", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.tan(degrees)),
    "toInt" -> (FunctionExpr("toInt", Seq(RefExpr("number", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, double : Double) => double.toInt)
  )

  lazy val evaluatorEnv : evaluating.Env = primitiveEnv.map(t => t._1 -> t._2._2)

  // String types plus primitive operations plus printer operations
  lazy val parserEnv : ParserEnv =
    ParserEnv.ofMap(stringTypeMap ++ primitiveEnv.map(t => t._1 -> t._2._1)) ++ Printer.toParserEnv

}
