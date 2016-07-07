package com.repocad.reposcript.parsing

import com.repocad.remote.HttpClient
import com.repocad.reposcript.Compiler
import com.repocad.reposcript.lexing.TokenLexer
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

trait ParsingTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val emptyEnv = Compiler.defaultEnv
  val mockClient = mock[HttpClient]
  var parser: Parser = null

  before {
    parser = new Parser(mockClient, emptyEnv, TokenLexer.lex(_, toLowerCase = true))
  }

  def testEqualsAll(expected: Expr, expression: String) = {
    parseStringAll(expression).right.map(_.expr) should equal(Right(expected))
  }

  def testEquals(expected: Expr, expression: String, env: ParserEnv = emptyEnv): Unit = {
    val either = parseString(expression, env).right.map(_.expr)
    either should equal(Right(expected))
  }

  def parseString(string: String, env: ParserEnv = emptyEnv, spillEnvironment: Boolean = false): Value[ExprState] = {
    parser = new Parser(mockClient, env, TokenLexer.lex(_, toLowerCase = true))
    parser.parse(string, spillEnvironment).fold(error => Left(error), state => Right(state))
  }

  def parseStringAll(string: String, env: ParserEnv = emptyEnv, spillEnvironment: Boolean = false) = {
    parser = new Parser(mockClient, env, TokenLexer.lex(_, toLowerCase = true))
    parser.parse(string, spillEnvironment).fold(error => Left(error), state => Right(state))
  }

}
