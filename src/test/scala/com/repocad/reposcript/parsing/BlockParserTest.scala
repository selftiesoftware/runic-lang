package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._

class BlockParserTest extends ParsingTest {

  private val stateTailer: ParserFunction[ExprState] =
    (state, success, failure) => success.apply(state.copy(tokens = state.tokens.tail))
  val mockedSuccess = mock[SuccessCont[ExprState]]
  val mockedFailure = mock[FailureCont[ExprState]]

  "A block parser" should "parse until a condition is met" in {
    val tokens = Lexer.lex("a b c")
    (mockedSuccess.apply _).expects(where { (state: ExprState) => state.tokens.head.toString == "'c" }).once()
    parser.parseUntil[ExprState](ExprState(UnitExpr, ParserEnv(), tokens),
      _.tokens.head.compare(SymbolToken("c")(Position.empty)) == 0, (first, second) => second,
      (state, success, failure) => success.apply(state.copy(tokens = state.tokens.tail)), mockedSuccess, mockedFailure)
  }
  it should "parse until a character token" in {
    val tokens = Lexer.lex("a b c")
    (mockedSuccess.apply _).expects(where { (state: ExprState) => {
      state.tokens.head.toString == "'b"
    }
    }).once()
    parser.parseUntilToken[ExprState](ExprState(UnitExpr, ParserEnv(), tokens), "b", (first, second) => second,
      stateTailer, mockedSuccess, mockedFailure)
  }
  it should "accumulate states" in {
    val tokens = Lexer.lex("1 2 3")
    (mockedSuccess.apply _).expects(ExprState(UnitExpr, ParserEnv(), LiveStream()))
    parser.parseUntilToken[ExprState](ExprState(UnitExpr, ParserEnv(), tokens), "3", (first, second) => second,
      stateTailer, mockedSuccess, mockedFailure)
  }

}
