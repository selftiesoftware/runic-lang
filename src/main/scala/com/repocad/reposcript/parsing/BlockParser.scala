package com.repocad.reposcript.parsing

/**
  * Parses blocks of code.
  */
trait BlockParser {

  def parseUntil[T <: ParserState](startState: T, condition: T => Boolean,
                                   parseFunction: ParserFunction[T],
                                   success: SuccessCont[T],
                                   failure: FailureCont[T]): Value[T] = {
    /*
    Implementation note: This is done procedurally to avoid stack overflows with too deep recursion.
     */
    var currentState: Value[T] = Right[Error, T](startState)
    while (currentState.isRight && !currentState.right.get.tokens.isPlugged && condition(currentState.right.get)) {
      parseFunction(currentState.right.get, (state: T) => {
        currentState = Right(state)
        currentState
      }, failure)
    }
    currentState match {
      case Left(error) => failure(error)
      case Right(state) => success(state)
    }
  }

  def parseUntilToken[T <: ParserState](state: T, token: String, parseFunction: ParserFunction[T],
                                        success: SuccessCont[T], failure: FailureCont[T]): Value[T] = {
    parseUntil[T](state, _.tokens.head.tag.toString == token, parseFunction, success, failure)
  }

  /*
  def parseBlock(parseCallback: (BlockState, SuccessCont, FailureCont) => Value,
                 condition: LiveStream[Token] => Boolean, beginningState: BlockState,
                 success: BlockState => Either[Error, BlockState], failure: FailureCont, spillEnvironment: Boolean = false):
  Either[Error, BlockState] = {
    var stateVar = beginningState
    var seqFail: Option[Error] = None
    def seqSuccess: SuccessCont = state => {
      stateVar = state
      Right(stateVar)
    }
    def seqFailure: FailureCont = (s) => {
      seqFail = Some(s)
      Left(s)
    }
    while (seqFail.isEmpty && !stateVar.tokens.isPlugged && !condition(stateVar.tokens)) {
      parseCallback(stateVar, seqSuccess, seqFailure) match {
        case Left(s) => seqFail = Some(s)
        case Right(newState) =>
          if (newState. != UnitExpr) {
            seqExpr :+= newState.expr
          }
          stateVar = newState
      }
    }
    if (!stateVar.tokens.isPlugged && condition(stateVar.tokens)) {
      stateVar = stateVar.copy(tokens = stateVar.tokens.tail)
    }
    seqFail.map(seqFailure).getOrElse(
      if (spillEnvironment) {
        success(stateVar.copy(expr = BlockExpr(seqExpr)))
      } else {
        success(stateVar.copy(expr = BlockExpr(seqExpr), env = beginningState.env))
      }
    )
  }*/

}
