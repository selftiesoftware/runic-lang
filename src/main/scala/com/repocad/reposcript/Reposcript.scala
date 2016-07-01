package com.repocad.reposcript

import java.net.URL

import com.repocad.remote._
import com.repocad.reposcript.lexing.TokenLexer
import com.repocad.reposcript.parsing.{Expr, Parser, ParserError}
import sun.net.www.protocol.http.HttpURLConnection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

/**
  * Commandline entry-point to Reposcript. See [[Reposcript.main()]] for more information.
  */
object Reposcript {

  private lazy val nativeHttpClient = new HttpClient {
    override def apply[T](url: String, method: HttpMethod, headers: Map[String, String], f: Response => T):
    Future[T] = {
      Future {
        method match {
          case Post(_) => throw new IllegalArgumentException("Post calls not supported")
          case Get =>
            val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
            connection.setRequestMethod(method.method)
            headers.foreach(header => connection.setRequestProperty(header._1, header._2))
            val text = Source.fromInputStream(connection.getInputStream).mkString("\n")
            f(Response(200, 0, text))
        }
      }
    }

  }

  def compile(file: Source, httpClient: HttpClient): Either[ParserError, Expr] = {
    val text = file.mkString("\n")
    Parser.parse(TokenLexer.lex(text), httpClient)
  }

  //  def evaluate(ast: Expr, httpClient: HttpClient, renderer: Renderer): Unit = {
  //
  //  }

  /**
    * Executes Reposcript in either compile (default), evaluate or full-stack mode.
    *
    * @param args Arguments for the application.
    */
  def main(args: Array[String]): Unit = {
    if (args.isEmpty) {
      printError("No arguments given")
    } else {
      parseFlags(args.toList)
    }
  }

  private def parseFlags(flags: List[String]): Unit = {
    flags match {
      case "help" :: tail => printHelp()
      //      case "evaluate" :: tail =>
      case "compile" :: file :: tail => compile(Source.fromFile(file, "UTF8"), nativeHttpClient).merge.toString
      case e => printError(s"Unknown command '$e'")
    }
  }

  private def printError(error: String): Unit = {
    println(error)
    printHelp()
  }

  private def printHelp(): Unit = {
    println("Usage: Reposcript [compilehelp] [input file]")
    println("\tcompile\tCompiles an input file by lexing and parsing it. Produces an AST.")
    println("\t\t\tRequires an input.")
    //    println("\tevaluate\tEvaluates an AST as a graphical model and renders it to a given output.")
    //    println("\t\t\tRequires an AST and output format.")
  }

}
