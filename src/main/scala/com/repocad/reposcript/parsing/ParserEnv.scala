package com.repocad.reposcript.parsing

/**
 * A parser environment
 */
case class ParserEnv(private val innerEnv : Map[String, Map[AnyType, Expr]]) {

  def +(kv : (String, Expr)) = {
    val inner = innerEnv.getOrElse(kv._1, Map[AnyType, Expr]()) + (kv._2.t -> kv._2)
    new ParserEnv(innerEnv + (kv._1 -> inner))
  }

  def ++(kvs : Traversable[(String, Expr)]) : ParserEnv = {
    kvs.foldLeft(this)((env, kv) => env.+(kv))
  }

  def ++(key : String, kvs : Traversable[(AnyType, Expr)]) : ParserEnv = {
    new ParserEnv(innerEnv + (key -> innerEnv.get(key).map(_ ++ kvs).getOrElse(kvs.toMap)))
  }

  def ++(kvs : ParserEnv) : ParserEnv = {
    kvs.toMap.foldLeft(this)((thisEnv, thatEnv) => thisEnv ++ (thatEnv._1, thatEnv._2))
  }

  def get(key : String) : Option[Expr] = innerEnv.get(key).flatMap(_.headOption.map(_._2))

  def getAsType(key : String, t : AnyType) : Option[Expr] = innerEnv.get(key).flatMap(_.find(_._1 == t).map(_._2))

  def getType(key : String) : Option[AnyType] = innerEnv.get(key)
    .map(map => map.find(overloaded => overloaded._2.isInstanceOf[AnyType]).map(_._2).asInstanceOf[AnyType])

  def -(key : String, t : AnyType) : ParserEnv = innerEnv.get(key).map(_.-(t))
    .map(newInnerMap => innerEnv + (key -> newInnerMap)).map(new ParserEnv(_)).getOrElse(this)

  def toMap = innerEnv

}

object ParserEnv {

  def apply() : ParserEnv  = new ParserEnv(Map())

  def apply(key : String, expr : Expr) : ParserEnv = ofMap(Map[String, Expr](key -> expr))

  def ofMap(map : Map[String, Expr]) : ParserEnv  = new ParserEnv(map.map(t => t._1 -> Map(t._2.t -> t._2)))

}
