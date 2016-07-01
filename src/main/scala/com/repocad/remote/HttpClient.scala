package com.repocad.remote

import scala.concurrent.{ExecutionContext, Future}

/**
  * An interface that can execute a request to a remote web service.
  */
abstract class HttpClient(implicit val executionContext: ExecutionContext) {

  /**
    * Performs a HTTP GET request to the given URL.
    *
    * @param url The url to send the request to.
    * @return A [[Response]].
    */
  def get(url: String): Future[Response] = {
    apply(url, Get, Map())
  }

  /**
    * Performs a HTTP GET request to the given URL.
    *
    * @param url The url to send the request to.
    * @param f   A function to map the result. Useful to avoid importing [[scala.concurrent.ExecutionContext]]s.
    *            Defaults to the identity function.
    * @return A [[Response]].
    */
  def get[T](url: String, f: Response => T): Future[T] = {
    apply(url, Get, Map(), f)
  }

  /**
    * Performs a HTTP POST request to the given URL.
    *
    * @param url  The URL to send the request to.
    * @param data The data to send with the request. Defaults to an empty string.
    * @return A [[Response]].
    */
  def post(url: String, data: String = ""): Future[Response] = {
    apply(url, Post(data), Map("Content-length" -> data.length.toString))
  }

  /**
    * Sends a HTTP request asynchronously to a given URL.
    *
    * @param url     The URL to send the request to.
    * @param method  The method to send the request with.
    * @param headers Headers to include in the request. Can be empty.
    * @return A [[Future]] that will contain a [[Response]].
    */
  def apply(url: String, method: HttpMethod, headers: Map[String, String]): Future[Response] = {
    apply[Response](url, method, headers, r => r)
  }

  /**
    * Sends a HTTP request and maps it to another type. Useful to abstract away imports of
    * [[scala.concurrent.ExecutionContext]]s.
    *
    * @param url     The URL to send the request to.
    * @param method  The method to send it with.
    * @param headers The headers of the request. Can be empty.
    * @param f       A function to map the Response.
    * @tparam T The type of the mapped value.
    */
  def apply[T](url: String, method: HttpMethod, headers: Map[String, String], f: Response => T): Future[T]

}
