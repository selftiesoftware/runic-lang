package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._

class BlockParserTest extends ParsingTest {

  "A block parser" should "parse until a condition is met" in {
    val tokens = Lexer.lex("a b c")
    val mockedSuccess = mock[SuccessCont[ExprState]]
    val mockedFailure = mock[FailureCont[ExprState]]
    (mockedSuccess.apply _).expects(where { (state: ExprState) => state.tokens.head.toString == "'c" }).once()
    parser.parseUntil[ExprState](ExprState(UnitExpr, ParserEnv(), tokens),
      _.tokens.head.compare(SymbolToken("c")(Position.empty)) == 0, (first, second) => second,
      (state, success, failure) => success.apply(state.copy(tokens = state.tokens.tail)), mockedSuccess, mockedFailure)
  }
}
