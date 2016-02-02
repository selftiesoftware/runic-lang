package com.repocad.reposcript.parsing

/**
  * Verifies typesafety and parameter types.
  */
trait TypedParser {

  def verifyTypeExists(typeName : String, state : ParserState) : Either[Error, AnyType] = {
    state.env.get(typeName) match {
      case Right(anyType : AnyType) => Right(anyType)
      case Right(other) => Left(Error.TYPE_NOT_FOUND(typeName)(state.position))
      case Left(errorFunction) => Left(errorFunction.apply(state.position))
    }
  }

}
