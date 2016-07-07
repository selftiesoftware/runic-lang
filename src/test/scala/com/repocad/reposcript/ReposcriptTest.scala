package com.repocad.reposcript

import com.repocad.remote.HttpClient
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.model.{SeqModel, ArcModel, LineModel, FontMetrics}
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import scala.concurrent.ExecutionContext.Implicits.global

class ReposcriptTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  val mockClient = mock[HttpClient]
  val mockPrinter: Renderer = mock[Renderer]
  val mockFontMetrics = mock[FontMetrics]
  val parser = new Parser(mockClient, Compiler.defaultEnv, TokenLexer.lex)

  "Reposcript" should "call a drawing function" in {
    (mockPrinter.circle _).expects(0d, 0d, 20d).once()
    Reposcript.run("def a as Number = 10 \n def b = a + 10 \n circle(0 0 b)", mockClient, mockFontMetrics, mockPrinter)
  }
  it should "create an AST from code" in {
    Reposcript.compile("def a as String = \"Hello\"", mockClient) should equal (Right(DefExpr("a", StringExpr("Hello"))))
  }
  it should "create a model from AST" in {
    val coordinate = NumberExpr(2)
    (Reposcript
      .model(CallExpr("line", UnitType, Seq(coordinate, coordinate, coordinate, coordinate)), mockClient, mockFontMetrics)
      ) should equal(Right(SeqModel(Seq(LineModel(2, 2, 2, 2)))))
  }
  it should "evaluate a model on a renderer" in {
    val model = ArcModel(0, 1, 2, 3, 4)
    (mockPrinter.arc _).expects(0d, 1d, 2d, 3d, 4d).once()
    Reposcript.render(model, mockPrinter)
  }
  it should "run code through the entire pipeline and render the results" in {
    (mockPrinter.circle _).expects(0d, 0d, 10d).once()
    Reposcript.run("circle(0 0 10)", mockClient, mockFontMetrics, mockPrinter)
  }


}
