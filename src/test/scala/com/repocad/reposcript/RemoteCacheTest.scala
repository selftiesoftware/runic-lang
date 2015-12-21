package com.repocad.reposcript

import com.repocad.reposcript.lexing.{Position, Lexer, LiveStream, Token}
import com.repocad.reposcript.parsing._
import org.scalamock.scalatest.MockFactory
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class RemoteCacheTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {

  class NoArgParser extends Parser(mockClient, ParserEnv())

  val mockClient = mock[HttpClient]
  val mockParser : Parser = mock[NoArgParser]
  var cache : RemoteCache = null

  before {
    cache = new RemoteCache(mockClient)
  }

  "A remote cache" should "fetch scripts" in {
    (mockClient.getSynchronous _).expects("get/test").returning(Response(0, 4, "")).once()

    cache.get("test", Position.start, _ => Left(Error("", Position.start)))
  }
  it should "fail when finding scripts that do not exist" in {
    cache.contains("test") should equal(false)
  }
  it should "cache remote scripts" in {
    val result : Value = Right(ParserState(NumberExpr(10), ParserEnv(), LiveStream(Iterable[Token]())))

    (mockClient.getSynchronous _).expects("get/test").returning(Response(0, 4, "10")).once()
    (mockParser.parse(_ : LiveStream[Token], _ : Boolean)).expects(Lexer.lex("10"), true).returning(result).once()

    cache.get("test", Position.start, text => mockParser.parse(Lexer.lex(text), true)).right.get.expr should equal(NumberExpr(10))
    cache.get("test", Position.start, text => mockParser.parse(Lexer.lex(text), true)).right.get.expr should equal(NumberExpr(10))
  }

}
