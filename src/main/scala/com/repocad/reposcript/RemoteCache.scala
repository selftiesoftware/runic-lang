package com.repocad.reposcript

import com.repocad.reposcript.lexing.Lexer
import com.repocad.reposcript.parsing.Parser

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.Duration

/**
 * A parser that caches remote scripts.
 * @param httpClient The client to execute requests to remote URL's.
 * @param parser The parser to process incoming scripts.
 */
class RemoteCache(httpClient: HttpClient, parser : Parser)(implicit ec : ExecutionContext) {

  private var scriptCache : Map[String, parsing.Value] = Map()

  def contains(scriptName : String) : Boolean = scriptCache.contains(scriptName)

  def get(scriptName : String) : parsing.Value = {
      scriptCache.getOrElse(scriptName, download(scriptName))
  }

  private def download(scriptName : String) : parsing.Value = {
    val result : Future[Response] = httpClient.get("http://repocad.com/get/" + scriptName)

    val value = result map {
      case Response(_, 4, text) =>
        parser.parse(Lexer.lex(text))
      case xs => Left(s"Script $scriptName failed to load with error: $xs")
    }

    val compiled = Await.result(value, Duration(100, "millis"))
    scriptCache += (scriptName -> compiled)
    compiled
  }

}
