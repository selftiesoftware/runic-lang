package com.repocad.reposcript.parsing

/**
  * A parser environment that can store one or more expressions under a name (identifier). Each name can have 0 or more
  * (overloaded) expressions stored under that name. The type of the expression is used to identify overloaded values.
  * Because of the possibility to overload expressions, ``get`` operations from this environment risks to cause a
  * runtime error, if more than one expression matches.
  */
case class ParserEnv(private val innerEnv : Map[String, Map[AnyType, Expr]]) {

  def +(kv : (String, Expr)) : ParserEnv = {
    val key = kv._1
    val typ : AnyType = kv._2.t
    val value = kv._2
    val newOverloaded : Map[AnyType, Expr] = innerEnv.get(key) match {
      case None => Map(typ -> value)
      case Some(overloaded) => overloaded.filter(t => !typ.isChild(t._1)).+(typ -> value)
    }
    new ParserEnv(innerEnv.updated(key, newOverloaded))
  }

  def ++(kvs : Traversable[(String, Expr)]) : ParserEnv = {
    kvs.foldLeft(this)((env, kv) => env.+(kv))
  }

  def ++(thatEnv : ParserEnv) : ParserEnv = {
    thatEnv.toMap.foldLeft(this)((thisEnv, thatEntry) => {
      thisEnv.getAll(thatEntry._1).foldLeft(thisEnv)(
        (thatEnv : ParserEnv, thatExpr : Expr) => thatEnv.+(thatEntry._1, thatExpr))
    })
  }

  def contains(name: String) = innerEnv.get(name).exists(_.nonEmpty)

  def get(key : String) : Option[Expr] = innerEnv.get(key).flatMap(_.headOption.map(_._2))

  def getAll(key : String) : Iterable[Expr] = innerEnv.get(key).map(_.values).getOrElse(Iterable[Expr]())

  def getAsType(key : String, typ : AnyType) : Option[Expr] = innerEnv.get(key).flatMap(_.find(t => typ.isChild(t._1))).map(_._2)
  def getAsType(key : String, f : AnyType => Boolean): Option[Expr] = innerEnv.get(key).flatMap(_.find(t => f(t._1)).map(_._2))

  def -(key : String, typ : AnyType) : ParserEnv = {
    val newInner : Map[String, Map[AnyType, Expr]] = innerEnv.get(key).map(_.filter(t => !typ.isChild(t._1))) match {
      case None => innerEnv
      case Some(xs : Map[AnyType, Expr]) if xs.isEmpty => innerEnv - key
      case Some(xs : Map[AnyType, Expr]) => innerEnv.updated(key, xs)
    }
    new ParserEnv(newInner)
  }

  def toMap = innerEnv

}

object ParserEnv {

  def apply() : ParserEnv  = empty

  def apply(kvs : (String, Expr)*) : ParserEnv = ofMap(kvs.toMap)

  val empty = new ParserEnv(Map())

  def ofMap(map : Map[String, Expr]) : ParserEnv  = new ParserEnv(map.map(t => t._1 -> Map(t._2.t -> t._2)))

}
