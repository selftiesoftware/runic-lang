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
    "+" -> (FunctionType("+", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env: evaluating.Env, a: Any, b: Any) => RepoMath.plus(a, b)),
    "-" -> (FunctionType("-", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.minus(a, b)),
    "*" -> (FunctionType("*", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.times(a, b)),
    "/" -> (FunctionType("/", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), NumberTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.divide(a, b)),
    "<" -> (FunctionType("<", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThan(a, b)),
    "<=" -> (FunctionType("<=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThanEquals(a, b)),
    ">" -> (FunctionType(">", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThan(b, a)),
    ">=" -> (FunctionType(">=", Seq(RefExpr("first", NumberType), RefExpr("second", NumberType)), BooleanTypeExpr),
      (env : evaluating.Env, a : Any, b : Any) => RepoMath.lessThanEquals(b, a)),
  // Trigonometry1
    "cos" -> (FunctionType("cos", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.cos(degrees)),
    "degrees" -> (FunctionType("degrees", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.toDegrees(degrees)),
    "sin" -> (FunctionType("sin", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.sin(degrees)),
    "radians" -> (FunctionType("radians", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.toRadians(degrees)),
    "tan" -> (FunctionType("tan", Seq(RefExpr("degrees", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, degrees : Double) => math.tan(degrees)),
    "toInt" -> (FunctionType("toInt", Seq(RefExpr("number", NumberType)), NumberTypeExpr),
      (_ : evaluating.Env, double : Double) => double.toInt)
  )

  lazy val evaluatorEnv : evaluating.Env = primitiveEnv.map(t => t._1 -> t._2._2)

  // String types plus primitive operations plus printer operations
  lazy val parserEnv : ParserEnv =
    ParserEnv.ofMap(stringTypeMap ++ primitiveEnv.map(t => t._1 -> t._2._1)) ++ Printer.toParserEnv

}
