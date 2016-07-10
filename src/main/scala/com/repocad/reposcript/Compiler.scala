package com.repocad.reposcript

import com.repocad.remote.HttpClient
import com.repocad.reposcript.lexing.{TokenLexer, Lexer}
import com.repocad.reposcript.parsing._

object Compiler {

  val vectorType = ObjectType("vector", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType)), AnyType)

  val stringTypeMap: Map[String, AnyType] = Map(
    "any" -> AnyType,
    "boolean" -> BooleanType,
    "number" -> NumberType,
    "string" -> StringType,
    "unit" -> UnitType
  )

  val defaultEnv: ParserEnv = ParserEnv(
    "vector" -> vectorType,
    "arc" -> FunctionType("arc", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("r", NumberType), RefExpr("sAngle", NumberType), RefExpr("eAngle", NumberType)), UnitExpr),
    "bezier" -> FunctionType("bezier", Seq(RefExpr("x1", NumberType), RefExpr("y1", NumberType), RefExpr("x2", NumberType), RefExpr("y2", NumberType), RefExpr("x3", NumberType), RefExpr("y3", NumberType), RefExpr("x4", NumberType), RefExpr("y4", NumberType)), UnitExpr),
    "circle" -> FunctionType("circle", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("r", NumberType)), UnitExpr),
    "line" -> FunctionType("line", Seq(RefExpr("x1", NumberType), RefExpr("y1", NumberType), RefExpr("x2", NumberType), RefExpr("y2", NumberType)), UnitExpr),
    "text" -> FunctionType("text", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("h", NumberType), RefExpr("t", AnyType)), vectorType),
    "text" -> FunctionType("text", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType), RefExpr("h", NumberType), RefExpr("t", AnyType), RefExpr("font", StringType)), vectorType)
  ) ++ stringTypeMap ++ Environment.primitiveEnv.map(t => t._1 -> t._2._1)

  /**
    * Parses some tokens into an AST of [[Expr]].
    *
    * @param code       The code to parse.
    * @param httpClient The HttpClient to use when importing modules.
    * @param defaultEnv The default environment to use when starting the parsing.
    * @param lexer      The lexer to employ when importing modules.
    * @return Either an [[ParserError]] or an [[Expr]].
    */
  def parse(code: String, httpClient: HttpClient, defaultEnv: ParserEnv = defaultEnv,
            lexer: Lexer = TokenLexer.lex): Either[ParserError, Expr] = {
    new Parser(httpClient, defaultEnv, lexer).parse(code, spillEnvironment = false).right.map(_.expr)
  }
}
