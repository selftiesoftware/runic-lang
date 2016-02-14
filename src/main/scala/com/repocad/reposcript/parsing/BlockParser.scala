package com.repocad.reposcript.parsing

/**
  * Parses blocks of code.
  */
trait BlockParser {

  def parseUntil[T <: ParserState](startState: T, condition: T => Boolean,
                                   accumulate: (T, T) => T,
                                   parseFunction: ParserFunction[T],
                                   success: SuccessCont[T],
                                   failure: FailureCont[T]): Value[T] = {
    /*
    Implementation note: This is done procedurally to avoid stack overflows with too deep recursion.
     */
    var currentState: Value[T] = Right[Error, T](startState)
    while (currentState.isRight && !currentState.right.get.tokens.isPlugged && !condition(currentState.right.get)) {
      parseFunction.apply(currentState.right.get, (state: T) => {
        currentState = Right(accumulate(currentState.right.get, state))
        currentState
      }, error => {
        currentState = Left(error)
        currentState
      })
    }
    currentState match {
      case Left(error) => failure(error)
      case Right(state) => success(state)
    }
  }

  def parseUntilToken[T <: ParserState](state: T, token: String, accumulate: (T, T) => T,
                                        parseFunction: ParserFunction[T], success: SuccessCont[T],
                                        failure: FailureCont[T]): Value[T] = {
    def stripToken(string: String): String = string match {
      case symbol if symbol.startsWith("'") => symbol.substring(1)
      case x => x
    }
    parseUntil[T](state, state => stripToken(state.tokens.head.toString) == token, accumulate,
      parseFunction, success, failure)
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
