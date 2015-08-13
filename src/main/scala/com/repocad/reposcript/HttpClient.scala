package com.repocad.reposcript

import scala.concurrent.Future

/**
 * A response to an Ajax request.
 * @param status The status code.
 * @param state The state of the request.
 * @param response The textual response.
 */
case class Response(status : Int, state : Int, response : String)

/**
 * An interface that can execute a request to a remote web service.
 */
trait HttpClient {

  /**
   * Performs a HTTP GET request to the given URL.
   * @param url The url to send the request to.
   * @return A [[Response]].
   */
  def get(url : String) : Future[Response] = {
    apply("GET", url, "", Map())
  }

  /**
   * Performs a HTTP GET request to the given URL and waits for the reply.
   * @param url The url to send the request to.
   * @return A [[Response]].
   */
  def getSynchronous(url : String) : Response = {
    applySynchronous("GET", url, "", Map())
  }

  /**
   * Performs a HTTP POST request to the given URL.
   * @param url The URL to send the request to.
   * @param data The data to send with the request. Defaults to an empty string.
   * @return A [[Response]].
   */
  def post(url : String, data : String = "") : Future[Response] = {
    apply("POST", url, data, Map("Content-length" -> data.length.toString))
  }

  /**
   * Sends a HTTP request asynchronously to a given URL.
   * @param method The HTTP method to use.
   * @param url The URL to send the request to.
   * @param data The data to send in the request.
   * @param headers Headers to include in the request. Can be empty.
   * @return A [[Future]] that will contain a [[Response]].
   */
  def apply(method : String, url : String, data : String, headers : Map[String, String]) : Future[Response]

  /**
   * Sends a HTTP request to a given URL and waits for the reply.
   * @param method The HTTP method to use.
   * @param url The URL to send the request to.
   * @param data The data to send in the request.
   * @param headers Headers to include in the request. Can be empty.
   * @return A [[Response]].
   */
  def applySynchronous(method : String, url : String, data : String, headers : Map[String, String]) : Response

}
