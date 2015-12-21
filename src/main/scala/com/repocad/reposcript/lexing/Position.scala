package com.repocad.reposcript.lexing

/**
  * A position in a piece of code.
  * @param lineNumber The line number in the code
  */
case class Position(lineNumber : Int)
object Position {
  val empty = Position(-1)
  val end = Position(Int.MaxValue)
  val start = Position(0)
}