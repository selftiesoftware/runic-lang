package com.repocad.remote

/**
  * A method with which to send HTTP requests.
  */
sealed trait HttpMethod {
  val method: String
}

case object Get extends HttpMethod {
  val method = "GET"
}

case class Post(data: String) extends HttpMethod {
  val method = "POST"
}