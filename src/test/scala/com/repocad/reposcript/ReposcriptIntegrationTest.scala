package com.repocad.reposcript

import java.io._

import com.repocad.remote.HttpClient
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.model.FontMetrics
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

class ReposcriptIntegrationTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val mockClient = mock[HttpClient]
  val mockPrinter: Renderer = mock[Renderer]
  val mockFontMetrics = mock[FontMetrics]
  val parser = new Parser(mockClient, Environment.parserEnv, TokenLexer.lex(_, toLowerCase = true))

  "Reposcript" should "call a drawing function" in {
    (mockPrinter.circle _).expects(0d, 0d, 20d).once()
    val expr = parser.parse("def a as Number = 10 \n def b = a + 10 \n circle(0 0 b)").right.get.expr
    Evaluator.eval(expr, parser, Environment.evaluatorEnv, mockPrinter, mockFontMetrics)
  }

  "Reposcript command line" should "compile to AST" in {
    val out = new ByteArrayOutputStream()
    val in = new ByteArrayInputStream("10".getBytes("UTF8"))
    scala.Console.withIn(in) {
      scala.Console.withOut(out) {
        Main.main(Array("compile"))
        in.close()
        out.close()
        val outString = out.toString("UTF8")
        //    println(outString)
        outString should equal(NumberExpr(10).toString)
      }
    }
  }

}
