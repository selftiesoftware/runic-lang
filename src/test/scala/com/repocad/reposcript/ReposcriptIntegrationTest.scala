package com.repocad.reposcript

import java.io._

import com.repocad.remote.HttpClient
import com.repocad.reposcript.evaluating.Evaluator
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

class ReposcriptIntegrationTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val mockClient = mock[HttpClient]
  val mockPrinter: Renderer = mock[Renderer]
  val parser = new Parser(mockClient, Environment.parserEnv, TokenLexer.lex(_, toLowerCase = true))
  val evaluator = new Evaluator(parser, Environment.evaluatorEnv)

  "Reposcript" should "call a drawing function" in {
    (mockPrinter.circle _).expects(0d, 0d, 20d).once()
    val expr = parser.parse("def a as Number = 10 \n def b = a + 10 \n circle(0 0 b)").right.get.expr
    println(evaluator.eval(expr, mockPrinter))
  }

  "Reposcript command line" should "compile to AST" in {
    val out = new ByteArrayOutputStream()
    val in = new ByteArrayInputStream("10".getBytes("UTF8"))
    scala.Console.withIn(in) {
      scala.Console.withOut(out) {
        Reposcript.main(Array("compile"))
        in.close()
        out.close()
        val outString = out.toString("UTF8")
        //    println(outString)
        outString should equal(NumberExpr(10).toString)
      }
    }
  }

}
