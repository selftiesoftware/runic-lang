package com.repocad.reposcript

import com.repocad.reposcript.lexing.{Position, Token, LiveStream}

/**
 * The parsing package contains code for converting [[com.repocad.reposcript.lexing.Token]]s into an Abstract Syntax Tree
 * (AST), which is a tree structure with an [[com.repocad.reposcript.parsing.Expr]] as the only root.
 */
package object parsing {

  type Value = Either[Error, ParserState]

  type FailureCont = Error => Value
  type SuccessCont = ParserState => Value

  lazy val stringTypeMap : Map[String, AnyType] = Map(
    "boolean" -> BooleanType,
    "number" -> NumberType,
    "string" -> StringType,
    "unit" -> UnitType
  )

}
