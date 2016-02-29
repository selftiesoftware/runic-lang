package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.Position

/**
  * A parser environment that can store one or more expressions under a name (identifier). Each name can have 0 or more
  * (overloaded) expressions stored under that name. The type of the expression is used to identify overloaded values.
  * Because of the possibility to overload expressions, ``get`` operations from this environment risks to cause a
  * [[Error]], if more than one expression matches the query.
  */
sealed case class ParserEnv(innerEnv: Map[String, Map[AnyType, Expr]]) {

  def +(kv: (String, Expr)): ParserEnv = {
    val key = kv._1
    val typ: AnyType = kv._2 match {
      case anyType: AnyType => anyType
      case _ => kv._2.t
    }
    val value = kv._2
    val newOverloaded: Map[AnyType, Expr] = innerEnv.get(key) match {
      case None => Map(typ -> value)
      case Some(overloaded) => overloaded.filter(t => !typ.isChild(t._1)).+(typ -> value)
    }
    new ParserEnv(innerEnv.updated(key, newOverloaded))
  }

  def ++(kvs: Traversable[(String, Expr)]): ParserEnv = {
    kvs.foldLeft(this)((env, kv) => env.+(kv))
  }

  def ++(thatEnv: ParserEnv): ParserEnv = {
    thatEnv.toMap.foldLeft(this)((thisEnv, thatEntry) => {
      thatEnv.getAll(thatEntry._1).foldLeft(thisEnv)((thatEnv: ParserEnv, thatExpr: Expr) =>
        thatEnv.+(thatEntry._1 -> thatExpr)
      )
    })
  }

  def contains(name: String) = innerEnv.get(name).exists(_.nonEmpty)

  def get(key: String): Either[Position => Error, Expr] = getAsType(key, AnyType)

  def getAll(key: String): Iterable[Expr] = innerEnv.get(key).map(_.values).getOrElse(Iterable[Expr]())

  def getAsType(key: String, typ: AnyType): Either[Position => Error, Expr] = getAsType(key, t => typ.isChild(t))

  def getAsType(key: String, f: AnyType => Boolean): Either[Position => Error, Expr] = {
    innerEnv.get(key) match {
      case None => Left(position => Error.TYPE_NOT_FOUND(key)(position))
      case Some(map) =>
        val matches = map.filter(t => f(t._1))
        matches.size match {
          case 0 => Left(position => Error.TYPE_NOT_FOUND(key)(position))
          case 1 => Right(matches.head._2)
          case n => Left(position => Error.AMBIGUOUS_TYPES(key, matches)(position))
        }
    }
  }

  def getCallableWithParameters(key: String, params: Seq[Expr]): Either[Position => Error, CallableType] = {
    innerEnv.get(key) match {
      case None => Left(position => Error.TYPE_NOT_FOUND(key)(position))
      case Some(map) =>
        val matches = map.filter({
          case (function: FunctionType, _) => isSameTypes(function.params, params)
          case (obj: ObjectType, _) => isSameTypes(obj.params, params)
          case _ => false
        }).asInstanceOf[Map[CallableType, AnyType]]
        matches.size match {
          case 0 => Left(position => Error.REFERENCE_NOT_FOUND(key, Some(params))(position))
          case 1 => Right(matches.head._1)
          case n => Left(position => Error.AMBIGUOUS_TYPES(key, matches)(position))
        }
    }
  }

  private def isSameTypes(expected: Seq[RefExpr], actual : Seq[Expr]) : Boolean = {
    if (expected.size != actual.size) {
      false
    } else {
      for (i <- expected.indices) {
        println(expected(i), actual(i), expected(i).t.isChild(actual(i).t))
        if (!expected(i).t.isChild(actual(i).t)) {
          return false
        }
      }
      true
    }
  }

  def -(key: String, typ: AnyType): ParserEnv = {
    val newInner: Map[String, Map[AnyType, Expr]] = innerEnv.get(key).map(_.filter(t => !typ.isChild(t._1))) match {
      case None => innerEnv
      case Some(xs: Map[AnyType, Expr]) if xs.isEmpty => innerEnv - key
      case Some(xs: Map[AnyType, Expr]) => innerEnv.updated(key, xs)
    }
    new ParserEnv(newInner)
  }

  def toMap = innerEnv

}

object ParserEnv {

  def apply(): ParserEnv = empty

  def apply(kvs: (String, Expr)*): ParserEnv = new ParserEnv(
    kvs.foldLeft(Map[String, Map[AnyType, Expr]]())(
      (map, t) => map.updated(t._1, map.getOrElse(t._1, Map()).+(t._2.t -> t._2))
    ))

  val empty = new ParserEnv(Map())

  def ofMap(map: Map[String, Expr]): ParserEnv = empty.++(map)

}
