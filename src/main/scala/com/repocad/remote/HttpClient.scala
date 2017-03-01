package com.repocad.remote

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
  * An interface that can execute a request to a remote web service.
  */
abstract class HttpClient(protected[remote] val executionContext: ExecutionContext) {

  /**
    * Sends a HTTP request asynchronously to a given URL.
    *
    * @param url     The URL to send the request to.
    * @param method  The method to send the request with.
    * @param headers Headers to include in the request. Can be empty.
    * @return A [[Future]] that will contain a [[Response]].
    */
  def apply(url: String, method: HttpMethod, headers: Map[String, String]): HttpResponse[Response] = {
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
  def apply[T](url: String, method: HttpMethod, headers: Map[String, String], f: Response => T): HttpResponse[T]
  = new HttpResponse[T](this, httpCall[T](url, method, headers, f))

  /**
    * Implementation of a HTTP call to a url with a method and headers.
    *
    * @param url     The url to call.
    * @param method  The method with which to call the URL.
    * @param headers The headers of the call.
    * @param f       A function to map the result to another type.
    * @tparam T The type of the resulting call.
    * @return A future of the call.
    */
  protected def httpCall[T](url: String, method: HttpMethod, headers: Map[String, String], f: Response => T): Future[T]

  /**
    * Performs a HTTP GET request to the given URL.
    *
    * @param url The url to send the request to.
    * @return A [[Response]].
    */
  def get(url: String): HttpResponse[Response] = {
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
  def get[T](url: String, f: Response => T): HttpResponse[T] = {
    apply(url, Get, Map(), f)
  }

  /**
    * Performs a HTTP POST request to the given URL.
    *
    * @param url  The URL to send the request to.
    * @param data The data to send with the request. Defaults to an empty string.
    * @return A [[Response]].
    */
  def post(url: String, data: String = ""): HttpResponse[Response] = {
    apply(url, Post(data), Map("Content-length" -> data.length.toString))
  }

  /**
    * Waits for the result of the future.
    *
    * @param future The future containing a result at some point in time.
    * @return The result of the future.
    */
  def result[T](future: Future[T]): Try[T]

}

/**
  * A response from a [[HttpClient]] which can be turned into a [[Response]] which uses the [[HttpClient#result]]
  * implementation to wait for the result.
  *
  * @param httpClient The client with information about how to wait for a result.
  * @param future     The future of the response.
  * @tparam T The type of the element in the response.
  */
sealed class HttpResponse[T](httpClient: HttpClient, val future: Future[T]) {

  /**
    * Gets the result of the response.
    *
    * @return The result of the response.
    */
  def result: Try[T] = httpClient.result(future)

  /**
    * Maps the response to another value.
    *
    * @param f The function mapping the type T to R.
    * @tparam R The type of element to map to.
    * @return An object of type R.
    */
  def map[R](f: T => R): HttpResponse[R] = new HttpResponse[R](httpClient, future.map(f)(httpClient.executionContext))

}
