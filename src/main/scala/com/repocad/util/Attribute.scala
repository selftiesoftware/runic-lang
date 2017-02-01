package com.repocad.util

import java.util.Objects

/**
  * An attribute with a name and a value.
  *
  * @tparam T The type of the attribute.
  */
sealed trait Attribute[T] extends Ordered[Attribute[_]] {
  val name: String
  val value: T

  override def compare(that: Attribute[_]): Int = name.compareTo(that.name)

}

object Attribute {

  case class Colour(value: String) extends Attribute[String] {
    val name = "colour"
  }

}