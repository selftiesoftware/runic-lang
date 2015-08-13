package com.repocad.reposcript

import com.repocad.reposcript.lexing.{Token, LiveStream, Lexer}
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.util.DirectedGraph
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class RemoteCacheTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  class NoArgParser extends Parser(mockClient)

  val mockClient = mock[HttpClient]
  val mockParser = mock[NoArgParser]
  var cache : RemoteCache = null

  before {
    cache = new RemoteCache(mockClient, mockParser)
  }

  "A remote cache" should "fetch scripts" in {
    (mockClient.getSynchronous _).expects("get/test").returning(Response(0, 4, "")).once()
    (mockParser.parse(_ : LiveStream[Token])).expects(Lexer.lex("")).once()

    cache.get("test")
  }
  it should "fail when finding scripts that do not exist" in {
    cache.contains("test") should equal(false)
  }
  it should "cache remote scripts" in {
    val result : parsing.Value = Right[String, (Expr, ValueEnv, TypeEnv)]((IntExpr(10), Map(), null))

    (mockClient.getSynchronous _).expects("get/test").returning(Response(0, 4, "10")).once()
    (mockParser.parse(_ : LiveStream[Token])).expects(Lexer.lex("10")).returning(result).once()

    cache.get("test") should equal(result)
    cache.get("test") should equal(result)
  }

}
