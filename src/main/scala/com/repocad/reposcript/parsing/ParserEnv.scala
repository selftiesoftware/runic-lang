package com.repocad.reposcript.parsing

/**
 * A parser environment
 */
case class ParserEnv(private val innerEnv : Map[String, Map[AnyType, Expr]]) extends Map[String, Map[AnyType, Expr]] {

  def +(kv : (String, Expr)) = {
    val inner = innerEnv.getOrElse(kv._1, Map[AnyType, Expr]()) + (kv._2.t -> kv._2)
    new ParserEnv(innerEnv + (kv._1 -> inner))
  }

  override def +[B1 >: Map[AnyType, Expr]](kv: (String, B1)): Map[String, B1] = innerEnv + kv

  override def get(key: String): Option[Map[AnyType, Expr]] = innerEnv.get(key)

  def get(key : String, t : AnyType) = innerEnv.get(key).map(_.get(t))

  override def iterator: Iterator[(String, Map[AnyType, Expr])] = innerEnv.iterator

  override def -(key: String): Map[String, Map[AnyType, Expr]] = new ParserEnv(innerEnv.-(key))

  def -(key : String, t : AnyType) : ParserEnv = innerEnv.get(key).map(_.-(t))
    .map(newInnerMap => innerEnv + (key -> newInnerMap)).map(new ParserEnv(_)).getOrElse(this)

}

object Env {

  def apply() = new ParserEnv(Map())

}
