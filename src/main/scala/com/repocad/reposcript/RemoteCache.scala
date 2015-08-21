package com.repocad.reposcript

/**
 * A parser that caches remote scripts.
 * @param httpClient The client to execute requests to remote URL's.
 */
class RemoteCache(httpClient: HttpClient) {

  private var scriptCache : Map[String, parsing.Value] = Map()

  def contains(scriptName : String) : Boolean = scriptCache.contains(scriptName)

  def get(scriptName : String, parser : (String) => parsing.Value) : parsing.Value = {
    scriptCache.getOrElse(scriptName, download(scriptName, parser))
  }

  private def download(scriptName : String, parser : (String) => parsing.Value) : parsing.Value = {
    val result = httpClient.getSynchronous("get/" + scriptName) match {
      case Response(_, 4, text) => parser(text)
      case xs => Left(s"Script $scriptName failed to load with error: $xs")
    }

    scriptCache = scriptCache + (scriptName -> result)

    result
  }

}
