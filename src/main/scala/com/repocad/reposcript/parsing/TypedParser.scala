package com.repocad.reposcript.parsing

/**
  * Verifies typesafety and parameter types.
  */
trait TypedParser {

  def verifyTypeExists(typeName : String, state : ParserState[_]) : Either[ParserError, AnyType] = {
    state.env.get(typeName) match {
      case Right(anyType : AnyType) => Right(anyType)
      case Right(other) => Left(ParserError.TYPE_NOT_FOUND(typeName)(state.position))
      case Left(errorFunction) => Left(errorFunction.apply(state.position))
    }
  }

}
