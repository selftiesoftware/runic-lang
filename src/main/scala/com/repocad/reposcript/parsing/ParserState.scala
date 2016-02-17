package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.{LiveStream, Position, Token}

/**
  * A state in the parsing which contains some result to be used in future steps in the parser. The state is used to
  * represent input and output of parsing functions which typically has the signature
  * <pre>[[ParserState]] => [[ParserState]]</pre>.
  * A state can thus both be interpreted as the output of a parsing, but also as the input to any further parsing
  * steps. The default state of the [[Parser]] is the [[ExprState]] which parses normal expressions.
  *
  * @tparam T The type of the ParserState implementation.
  */
abstract class ParserState[T <: ParserState[T]] {

  def withTokens(tokens: LiveStream[Token] = tokens): T

  /**
    * The environment contained in this state.
    */
  val env: ParserEnv

  /**
    * The remaining tokens that can be parsed in this state.
    */
  val tokens: LiveStream[Token]

  /**
    * The [[Position]] of the source code in this state. Useful for debugging with information about the exact
    * line number.
    *
    * @return The [[Position]] in the source code of the next [[Token]]s in this state.
    */
  def position: Position = tokens.source.headOption.map(_.position).getOrElse(Position.end)


}

/**
  * A state in the parsing where [[Token]]s are evaluated to a definition.
  *
  * @param name                The name of the thing being defined.
  * @param parameters          A number of parameters given to the definition.
  * @param recursiveParameters Recursive parameters which are not yet evaluated to expressions.
  * @param env                 The environment of the state.
  * @param tokens              The remaining tokens to parse.
  */
case class DefinitionState(name: String, parameters: Seq[RefExpr], recursiveParameters: Seq[String], env: ParserEnv,
                           tokens: LiveStream[Token]) extends ParserState[DefinitionState] {
  override def withTokens(tokens: LiveStream[Token] = tokens): DefinitionState = copy(tokens = tokens)
}

/**
  * Helps construct [[DefinitionState]]s.
  */
object DefinitionState {
  def apply(name: String, env: ParserEnv, tokens: LiveStream[Token]): DefinitionState =
    DefinitionState(name, Seq(), Seq(), env, tokens)
}

/**
  * A state in the parsing of [[Token]]s to [[Expr]].
  *
  * @param expr   The expression that has been evaluated in this state.
  * @param env    The environment with the currenly stored values.
  * @param tokens The remaining tokens to parse.
  */
case class ExprState(expr: Expr, env: ParserEnv, tokens: LiveStream[Token]) extends ParserState[ExprState] {
  override def withTokens(tokens: LiveStream[Token] = tokens): ExprState = copy(tokens = tokens)
}

/**
  * Helps construct [[ParserState]] instantiations.
  */
object ExprState {
  private val emptyStream = LiveStream[Token](Iterable())

  def apply(expr: Expr, env: ParserEnv): ExprState = new ExprState(expr, env, emptyStream)
}
