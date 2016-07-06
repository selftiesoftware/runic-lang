package com.repocad.reposcript

import java.io._

import com.repocad.remote.HttpClient
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.model.FontMetrics
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class MainTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

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
