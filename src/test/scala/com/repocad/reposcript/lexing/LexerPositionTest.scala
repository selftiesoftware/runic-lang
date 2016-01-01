package com.repocad.reposcript.lexing

import org.scalatest.{Matchers, FlatSpec}

class LexerPositionTest extends FlatSpec with Matchers {

  "A lexer with position markers" should "register newlines" in {
    val tokens = Lexer.lex("a\nb")
    tokens.head.position should equal(Position(0))
    tokens.tail.head.position should equal(Position(1))
  }
  it should "register newlines after comments" in {
    Lexer.lex("#testing\na").head.position should equal(Position(1))
  }
  it should "register newlines after lines with appending comments" in {
    Lexer.lex("code #comment \ncode2").tail.head.position should equal(Position(1))
  }
  it should "register newlines after two new lines" in {
    Lexer.lex("\n\ncode").head.position should equal(Position(2))
  }

}
