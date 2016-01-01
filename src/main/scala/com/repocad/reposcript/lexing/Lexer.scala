package com.repocad.reposcript.lexing

/*
 SXLexer: A lexer for the programming language Scheme

 Author: Matthew Might
 Site:   http://matt.might.net/

 */
class Lexer extends NonblockingLexer[Char, Token] {

  import RegularLanguageImplicits._

  implicit def charsToString(l : List[Char]) : String = l.mkString

  private var position : Position = Position(0)

  // Abbreviations:
  private val ch = "#\\" ~ AnyChar
  private val id = (('A' thru 'Z') || ('a' thru 'z') || ('0' thru '9') || oneOf("-+/*_?%$#&^=!@<>:")).+
  private val int    = ("-"?) ~ ('0' thru '9').+
  private val double = ("-"?) ~ ('0' thru '9').+ ~ '.' ~ ('0' thru '9').+
  private val ws = oneOf(" \r\t").+ // whitespace
  private val newline = oneOf("\n") // newline
  private val comment = "\\\\" ~ ((!oneOf("\r\n"))*) // single-line comment
  private val hashComment = "#" ~ ((!oneOf("\r\n"))*) // hashtag comments

  // States:
  protected val MAIN       = State()
  private val MULTICOMMENT = State(0)
  private val STRING       = State[List[Char]](List())
  private val BLOCK        = State[List[Token]](List())

  // Rules:

  // State switching rules
  MAIN switchesOn "/*" to { MULTICOMMENT(1) }
  MAIN switchesOn "\"" to { STRING(List()) }

  // Regular tokens
  MAIN (comment)   { }
  MAIN (hashComment)   { }
  MAIN (",")   { emit(PunctToken(",")(position)) }
  MAIN ("`")   { emit(PunctToken("`")(position)) }
  MAIN ("'")   { emit(PunctToken("'")(position)) }
  MAIN ("(")   { emit(PunctToken("(")(position)) }
  MAIN (")")   { emit(PunctToken(")")(position)) }
  MAIN ("[")   { emit(PunctToken("[")(position)) }
  MAIN ("]")   { emit(PunctToken("]")(position)) }
  MAIN ("{")   { emit(PunctToken("{")(position)) }
  MAIN ("}")   { emit(PunctToken("}")(position)) }
  MAIN (".")   { emit(PunctToken(".")(position)) }
  MAIN (END)   { terminate() }
  MAIN (ws)    {  }
  MAIN (newline)    { incrementLine() }
  MAIN (int)   over { chars => emit(IntToken(Integer.parseInt(chars))(position)) }
  MAIN (double) over { chars => emit(DoubleToken(java.lang.Double.parseDouble(chars))(position))}
  MAIN (id)    over { chars => emit(SymbolToken(chars)(position)) }

  // Strings
  STRING ("\"")    = { (string : List[Char], _ : List[Char])     => { emit(StringToken(string.reverse.mkString)(position)) ; MAIN } }
  STRING ("\\\"")  = { (string : List[Char], chars : List[Char]) => STRING('"' :: string) }
  STRING ("\\n")   = { (string : List[Char], chars : List[Char]) => STRING('\n' :: string) }
  STRING ("\\\\")  = { (string : List[Char], chars : List[Char]) => STRING('\\' :: string) }
  STRING (AnyChar) = { (string : List[Char], chars : List[Char]) => STRING(chars.reverse ++ string) }

  // /* ... */ comments
  MULTICOMMENT ("/*")    = { (n : Int, chars : List[Char]) => MULTICOMMENT(n+1) }
  MULTICOMMENT (newline) { incrementLine() }
  MULTICOMMENT (AnyChar) { }
  MULTICOMMENT ("*/")    = { case (1,chars) => MAIN case (n : Int, chars) => MULTICOMMENT(n - 1) }

  def incrementLine(): Unit = {
    position = position.copy(lineNumber = position.lineNumber + 1)
  }

}

object Lexer {
  def lex(code : String) : LiveStream[Token] = {
    val stream = LiveStream(code)
    val lexer = new Lexer()
    lexer.lex(stream)
    lexer.output
  }
}

