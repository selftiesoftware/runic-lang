package com.repocad.reposcript

import com.repocad.remote.{HttpClient, Response}
import com.repocad.reposcript.lexing.Position
import com.repocad.reposcript.parsing.ExprState

import scala.concurrent.Future

/**
  * A parser that caches remote scripts.
  *
  * @param httpClient The client to execute requests to remote URL's.
  */
class RemoteCache(httpClient: HttpClient) {

  private var scriptCache: Map[String, Future[parsing.Value[ExprState]]] = Map()

  def contains(scriptName: String): Boolean = scriptCache.contains(scriptName)

  def get(scriptName: String, position: Position,
          parser: (String) => parsing.Value[ExprState]): Future[parsing.Value[ExprState]] = {
    scriptCache.getOrElse(scriptName, download(scriptName, position, parser))
  }

  private def download(scriptName: String, position: Position,
                       parser: (String) => parsing.Value[ExprState]): Future[parsing.Value[ExprState]] = {
    val result: Future[parsing.Value[ExprState]] = httpClient.get("get/" + scriptName, {
      case Response(_, 4, text) => parser(text)
      case xs => Left(parsing.ParserError.IMPORT_FAILED(scriptName, xs.toString)(position))
    })

    scriptCache = scriptCache + (scriptName -> result)

    result
  }

}
