//package com.repocad.reposcript
//
//import com.repocad.remote.{HttpClient, Response}
//import com.repocad.reposcript.lexing._
//import com.repocad.reposcript.parsing._
//import org.scalamock.scalatest.MockFactory
//import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
//
//class RemoteCacheTest extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {
//
//  class NoArgParser extends Parser(mockClient, ParserEnv(), TokenLexer.lex)
//
//  val mockClient = mock[HttpClient]
//  val mockParser: Parser = mock[NoArgParser]
//  var cache: RemoteCache = null
//
//  before {
//    cache = new RemoteCache(mockClient)
//  }
//
//  "A remote cache" should "fetch scripts" in {
//    (mockClient.get(_: String).value.get.get).expects("get/test").returning(Response(0, 4, "")).once()
//    cache.get("test", Position.start, _ => Left(ParserError("", Position.start)))
//  }
//  it should "fail when finding scripts that do not exist" in {
//    cache.contains("test") should equal(false)
//  }
//  it should "cache remote scripts" in {
//    val result: Value[ExprState] = Right(ExprState(NumberExpr(10), ParserEnv(), LiveStream(Iterable[Token]())))
//
//    (mockClient.get(_: String).value.get.get).expects("get/test").returning(Response(0, 4, "10")).once()
//    (mockParser.parse(_: LiveStream[Token], _: Boolean)).expects(TokenLexer.lex("10"), true).returning(result).once()
//
//    cache.get("test", Position.start, text => mockParser.parse(TokenLexer.lex(text), true))
//      .value.get.get.right.get.expr should equal(NumberExpr(10))
//    cache.get("test", Position.start, text => mockParser.parse(TokenLexer.lex(text), true))
//      .value.get.get.right.get.expr should equal(NumberExpr(10))
//  }
//
//}
