package com.repocad.reposcript

import com.repocad.reposcript.evaluating.Evaluator
import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class ReposcriptIntegrationTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val mockClient = mock[HttpClient]
  val mockPrinter: Renderer = mock[Renderer]
  val parser = new Parser(mockClient, Environment.parserEnv, Lexer.lex)
  val evaluator = new Evaluator(parser, Environment.evaluatorEnv)

  "Reposcript" should "parse a plus statement" in {
    (mockPrinter.circle _).expects(0d, 0d, 20d).once()
    val expr = parser.parse("def a as Number = 10 \n def b = a + 10 \n circle(0 0 b)").right.get.expr
    evaluator.eval(expr, mockPrinter)
  }
  it should "access an object element from a function" in {
    val expr = parser.parse("def Object(x as Number) \n def function(o as Object) = o.x \n function(Object(10))")
    evaluator.eval(expr.right.get.expr, mockPrinter).right.get._2 should equal(10)
  }

}
