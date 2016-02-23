package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.{Environment, HttpClient}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

trait ParsingTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val emptyEnv = Environment.parserEnv
  val mockClient = mock[HttpClient]
  var parser: Parser = null

  before {
    parser = new Parser(mockClient, emptyEnv, Lexer.lex)
  }

  def testEqualsAll(expected: Expr, expression: String) = {
    parseStringAll(expression).right.map(_.expr) should equal(Right(expected))
  }

  def testEquals(expected: Expr, expression: String, env: ParserEnv = emptyEnv): Unit = {
    parser = new Parser(mockClient, env, Lexer.lex)
    val either = parseString(expression, env).right.map(_.expr)
    either should equal(Right(expected))
  }

  def parseString(string: String, env: ParserEnv = emptyEnv): Value[ExprState] = {
    parser = new Parser(mockClient, env, Lexer.lex)
    val stream = Lexer.lex(string)
    parser.parse(ExprState(UnitExpr, env, stream), state => Right(state), f => Left(f))
  }

  def parseStringAll(string: String, env: ParserEnv = emptyEnv, spillEnvironment: Boolean = false) = {
    parser = new Parser(mockClient, env, Lexer.lex)
    val stream = Lexer.lex(string.toLowerCase)
    parser.parse(stream, spillEnvironment = spillEnvironment)
  }

}
