package com.repocad.reposcript

import com.repocad.reposcript.lexing.Position

/**
 * A parser that caches remote scripts.
 * @param httpClient The client to execute requests to remote URL's.
 */
class RemoteCache(httpClient: HttpClient) {

  private var scriptCache : Map[String, parsing.Value] = Map()

  def contains(scriptName : String) : Boolean = scriptCache.contains(scriptName)

  def get(scriptName : String, position : Position, parser : (String) => parsing.Value) : parsing.Value = {
    scriptCache.getOrElse(scriptName, download(scriptName, position, parser))
  }

  private def download(scriptName : String, position : Position, parser : (String) => parsing.Value) : parsing.Value = {
    val result : parsing.Value = httpClient.getSynchronous("get/" + scriptName) match {
      case Response(_, 4, text) => parser(text)
      case xs => Left(parsing.Error.IMPORT_FAILED(scriptName, xs.toString)(position))
    }

    scriptCache = scriptCache + (scriptName -> result)

    result
  }

}
