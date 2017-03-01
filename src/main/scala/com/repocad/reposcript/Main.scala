package com.repocad.reposcript

import java.io.BufferedReader
import java.net.URL
import java.util.concurrent.TimeUnit

import com.repocad.remote._
import com.repocad.reposcript.parsing.{Expr, ParserError}
import sun.net.www.protocol.http.HttpURLConnection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
  * Commandline entry-point to Reposcript. See [[Main.main()]] for more information.
  */
object Main {

  private lazy val nativeHttpClient = new HttpClient(scala.concurrent.ExecutionContext.Implicits.global) {
    override def httpCall[T](url: String, method: HttpMethod, headers: Map[String, String], f: Response => T):
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

    override def result[T](future: Future[T]): Try[T] = try {
      Success(Await.result(future, Duration(500, TimeUnit.MILLISECONDS)))
    } catch {
      case e: Exception => Failure(e)
    }
  }

  def compile(reader: BufferedReader, httpClient: HttpClient): Either[ParserError, Expr] = {
    val text = Stream.continually(reader.readLine()).takeWhile(_ != null).mkString("\n")
    Compiler.parse(text, httpClient)
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
      case "compile" :: "-i" :: file :: Nil =>
        print(compile(Source.fromFile(file, "UTF8").bufferedReader(), nativeHttpClient).merge.toString)
      case "compile" :: Nil =>
        print(compile(Console.in, nativeHttpClient).merge.toString)
      case e => printError(s"Unknown command '$e'")
    }
  }

  private def printError(error: String): Unit = {
    println("Error: " + error)
    printHelp()
  }

  private def printHelp(): Unit = {
    println("Usage: Reposcript [compile|help] [-i input file]")
    println("Reposcript is a library for lexing, parsing and interpreting the Reposcript language.")
    println("It can be used in two phases: compile and evaluate. By default the input is read from standard input.")
    println("Compilation: Lexes and parser text into an Abstract Syntax Tree (AST).")
    println("Evaluation: Evaluates the AST to a euclidean model and renders it on a graphical surface.")
    println("Options:")
    println("\tcompile\t\tCompiles an input file by lexing and parsing it. Produces an AST.")
    println("\t\t\tRequires an input.")
    println("Flags:")
    println("\t-i\t\tAn input file to read the input source from.")
    //    println("\tevaluate\tEvaluates an AST as a graphical model and renders it to a given output.")
    //    println("\t\t\tRequires an AST and output format.")
  }

}
