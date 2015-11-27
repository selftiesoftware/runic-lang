package com.repocad.reposcript.parsing

import com.repocad.reposcript.{Response, RemoteCache, HttpClient}

class ImportTest extends ParsingTest {

  "Import parsing" should "parse an import statement" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "10"))

    testEquals(ImportExpr("a"), "import a")
  }
  it should "include the imported environment for further parsing" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "def b = 10"))

    parseStringAll("import a b", ParserEnv(), spillEnvironment = true) should equal(
      Right(BlockExpr(Seq(ImportExpr("a"), RefExpr("b", NumberType))), ParserEnv("b" -> NumberExpr(10)))
    )
  }
  it should "import multiple scripts" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "def aVal = 10"))
    (mockClient.getSynchronous _).expects("get/b").returning(Response(0, 4, "def bVal = 20"))

    parseStringAll("import a import b aVal bVal", ParserEnv(), spillEnvironment = true) should equal(
      Right(BlockExpr(Seq(ImportExpr("a"), ImportExpr("b"), RefExpr("aVal", NumberType), RefExpr("bVal", NumberType))),
        ParserEnv("aVal" -> NumberExpr(10), "bVal" -> NumberExpr(20)))
    )
  }

}
