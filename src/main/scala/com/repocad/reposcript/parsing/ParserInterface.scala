package com.repocad.reposcript.parsing

/**
  * An interface for a general parser which can parse one [[Expr]] from a stream of
  * [[com.repocad.reposcript.lexing.Token]]s.
  */
trait ParserInterface {

  /**
    * Parses the tokens inside the given [[ExprState]] and calls the success function whenever another state is
    * successfully extracted or the failure function if an error in the syntax or a violation of type-safety is
    * encountered.
    *
    * @param state   The state of the parser before parsing.
    * @param success A function to call when an [[Expr]] is succesfully parsed.
    * @param failure A function to call when an error is encountered.
    * @return [[Either]] an error on failure ([[Left]]) or an [[Expr]] on success ([[Right]]).
    */
  def parse(state: ExprState, success: SuccessCont[ExprState], failure: FailureCont[ExprState]): Value[ExprState]

}
