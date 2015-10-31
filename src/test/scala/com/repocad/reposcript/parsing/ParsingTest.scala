package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.{Environment, HttpClient}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

trait ParsingTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val emptyEnv = Environment.parserEnv
  val mockClient = mock[HttpClient]
  var parser : Parser = null

  before {
    parser = new Parser(mockClient, emptyEnv)
  }

  def testEqualsAll(expected : Seq[Expr], expression : String) = {
    parseStringAll(expression).right.map(_._1) should equal(Right(BlockExpr(expected)))
  }

  def testEquals(expected : Expr, expression : String, env: ParserEnv = emptyEnv) = {
    parser = new Parser(mockClient, env)
    val either = parseString(expression, env).right.map(_._1)
    either should equal(Right(expected))
  }

  def parseString(string : String, env: ParserEnv = emptyEnv) : Value = {
    parser = new Parser(mockClient, env)
    val stream = Lexer.lex(string)
    parser.parse(stream, env, (t, newEnv, _) => Right((t, newEnv)), f => Left(f))
  }

  def parseStringAll(string : String, env : ParserEnv = emptyEnv) = {
    parser = new Parser(mockClient, env)
    val stream = Lexer.lex(string)
    parser.parse(stream)
  }

}
