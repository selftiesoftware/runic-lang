package com.repocad.reposcript.parsing

import com.repocad.reposcript.Response

class ImportTest extends ParsingTest {

  "Import parsing" should "parse an import statement" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "10"))

    testEquals(ImportExpr("a"), "import a")
  }
  it should "include the imported environment for further parsing" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "def b = 10"))

    parseStringAll("import a b", ParserEnv(), spillEnvironment = true) should equal(
      Right(ExprState(BlockExpr(Seq(ImportExpr("a"), RefExpr("b", NumberType))), ParserEnv("b" -> NumberExpr(10))))
    )
  }
  it should "import multiple scripts" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "def aval = 10"))
    (mockClient.getSynchronous _).expects("get/b").returning(Response(0, 4, "def bval = 20"))

    parseStringAll("import a import b aval bval", ParserEnv(), spillEnvironment = true) should equal(
      Right(ExprState(BlockExpr(Seq(ImportExpr("a"), ImportExpr("b"), RefExpr("aval", NumberType), RefExpr("bval", NumberType))),
        ParserEnv("aval" -> NumberExpr(10), "bval" -> NumberExpr(20))))
    )
  }

}
