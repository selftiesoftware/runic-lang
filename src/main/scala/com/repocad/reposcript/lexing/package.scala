package com.repocad.reposcript

/**
  * A package for lexing Strings to [[com.repocad.reposcript.lexing.Token]]s.
  */
package object lexing {

  type Lexer = String => LiveStream[Token]

}
