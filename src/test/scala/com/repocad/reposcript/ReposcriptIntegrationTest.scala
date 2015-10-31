package com.repocad.reposcript

import com.repocad.reposcript.evaluating.Evaluator
import com.repocad.reposcript.lexing.{Lexer, LiveStream, Token}
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class ReposcriptIntegrationTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val mockClient = mock[HttpClient]
  val mockPrinter : Printer[_] = mock[Printer[Any]]
  val parser = new Parser(mockClient, ParserEnv())
  val evaluator = new Evaluator(parser, Environment.evaluatorEnv)

  "Reposcript" should "parse a plus statement" in {
    (mockPrinter.circle _).expects(0d, 0d, 20d).once()
    val expr = parser.parse(Lexer.lex("def a as Number = 10 \n def b = a + 10 \n circle(0 0 b)")).right.get._1
    evaluator.eval(expr, mockPrinter)
  }

}
