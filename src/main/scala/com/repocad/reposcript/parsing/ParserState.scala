package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.{LiveStream, Position, Token}

/**
  * A state in the parsing which contains results of a generic type [[T]]. The state is used to represent input and
  * output of parsing functions which typically has the signature <pre>[[ParserState]] => [[ParserState]]</pre>.
  * A state can thus both be interpreted as the output of a parsing, but also as the input to any further parsing
  * steps.
  *
  * @tparam T The type of the results in this state.
  */
abstract class ParserState[T] {

  /**
    * The content of the state. This is typically used as the result of a parsing function.
    */
  abstract val content: T

  /**
    * The environment contained in this state.
    */
  abstract val env: ParserEnv

  /**
    * The remaining tokens that can be parsed in this state.
    */
  abstract val tokens: LiveStream[Token]

  /**
    * The [[Position]] of the source code in this state. Useful for debugging with information about the exact
    * line number.
    *
    * @return The [[Position]] in the source code of the next [[Token]]s in this state.
    */
  def position: Position = tokens.source.headOption.map(_.position).getOrElse(Position.end)

}

case class RecursiveState[T <: Expr](content: T => Seq[RefExpr], env: ParserEnv, tokens: LiveStream[Token]) extends ParserState[T => Seq[RefExpr]]

/**
  * A state in the parsing of [[Token]]s to [[Expr]].
  *
  * @param expr   The expression that has been evaluated in this state.
  * @param env    The environment with the currenly stored values.
  * @param tokens The remaining tokens to parse.
  */
case class ExprState(expr: Expr, env: ParserEnv, tokens: LiveStream[Token]) extends ParserState[Expr] {
  def content = expr
}

/**
  * Helps construct [[ParserState]] instantiations.
  */
object ParserState {
  private val emptyStream = LiveStream[Token](Iterable())

  def apply(expr: Expr, env: ParserEnv): ParserState[Expr] = new ExprState(expr, env, emptyStream)
}
