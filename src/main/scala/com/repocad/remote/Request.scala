package com.repocad.remote

/**
  * A request made to the backend.
  *
  * @param url     The http url to send the request to. Should start with "http(s)".
  * @param method  The [[HttpMethod]] with which to send the request.
  * @param headers The http headers of the request.
  */
case class Request(url: String, method: HttpMethod, headers: Map[String, String])

