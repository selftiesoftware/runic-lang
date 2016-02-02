package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.{Token, LiveStream}

/**
  * Parses blocks of code.
  */
trait BlockParser extends ParserInterface {

  def parseUntil(token : String, state : ParserState,
                         success : SuccessCont, failure: FailureCont): Value = {
    parseUntil(parse, stream => stream.head.tag.toString.equals(token), state, success, failure)
  }

  def parseUntil(parseCallback : (ParserState, SuccessCont, FailureCont) => Value,
                         condition : LiveStream[Token] => Boolean, beginningState : ParserState,
                         success : SuccessCont, failure : FailureCont, spillEnvironment : Boolean = false): Value = {
    var stateVar = beginningState
    var seqExpr : Seq[Expr] = Seq()
    var seqFail : Option[Error] = None
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
          if (newState.expr != UnitExpr) {
            seqExpr :+= newState.expr
          }
          stateVar = newState
      }
    }
    if (!stateVar.tokens.isPlugged && condition(stateVar.tokens) ) {
      stateVar = stateVar.copy(tokens = stateVar.tokens.tail)
    }
    seqFail.map(seqFailure).getOrElse(
      if (spillEnvironment) {
        success(stateVar.copy(expr = BlockExpr(seqExpr)))
      } else {
        success(stateVar.copy(expr = BlockExpr(seqExpr), env = beginningState.env))
      }
    )
  }

}
