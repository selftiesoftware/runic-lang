package com.repocad.reposcript.evaluating

import com.repocad.reposcript.parsing.{AnyType, Expr}

/**
  * An environment for the evaluator which discards irrelevant type information to keep the evaluation as optimal as
  * possible.
  */
case class EvaluatorEnv(map: Map[Signature, Any] = Map()) {

  def add(name: String, input: Seq[Expr], output: AnyType, value: Any): EvaluatorEnv = {
    new EvaluatorEnv(map + (Signature(name, input.map(_.t), output) -> value))
  }

  def -(name: String, input: Seq[Expr], output: AnyType): EvaluatorEnv = {
    new EvaluatorEnv(map.-(Signature(name, input.map(_.t), output)))
  }

  def ++(that: EvaluatorEnv): EvaluatorEnv = {
    new EvaluatorEnv(map ++ that.map)
  }

  def get(name: String, input: Seq[Expr], output: AnyType): Option[Any] = {
    map.get(Signature(name, input.map(_.t), output))
  }

}

object EvaluatorEnv {
  def apply() = empty

  val empty = new EvaluatorEnv()
}

case class Signature(name: String, input: Seq[AnyType], output: AnyType)

