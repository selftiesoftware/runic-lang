package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.{Position, Token, LiveStream}

/**
  * A state in the parsing of [[Token]]s to [[Expr]].
  * @param expr The current expression being evaluated
  * @param env The environment with the currenly stored values
  * @param tokens The remaining tokens to parse
  */
case class ParserState(expr : Expr, env : ParserEnv, tokens : LiveStream[Token]) {
  def position : Position = tokens.source.headOption.map(_.position).getOrElse(Position.end)
}

object ParserState {
  private val emptyStream = LiveStream[Token](Iterable())

  def apply(expr : Expr, env : ParserEnv) : ParserState = ParserState(expr, env, emptyStream)
}
