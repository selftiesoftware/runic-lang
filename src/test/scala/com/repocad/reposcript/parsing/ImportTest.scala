package com.repocad.reposcript.parsing

import com.repocad.reposcript.{Response, RemoteCache, HttpClient}

class ImportTest extends ParsingTest {

  "Import parsing" should "parse an import statement" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "10"))

    testEquals(ImportExpr("a"), "import a")
  }
  it should "include the imported environment for further parsing" in {
    (mockClient.getSynchronous _).expects("get/a").returning(Response(0, 4, "def b = 10"))

    parseString("import a b", ParserEnv()) should equal(
      Right(ImportExpr("a"), ParserEnv("b", NumberExpr(10)))
    )
  }

}
