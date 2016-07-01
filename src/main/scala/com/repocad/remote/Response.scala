package com.repocad.remote

/**
  * A response to an Ajax request.
  *
  * @param status   The status code.
  * @param state    The state of the request.
  * @param response The textual response.
  */
case class Response(status: Int, state: Int, response: String)
