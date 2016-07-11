package com.repocad.geom
import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, BooleanOperators, all}

object TransformationMatrixSpecification extends Properties("TransformationMatrix") {
  val flt_epsilon = 1E-4

  //Frankly this is wrong. Epsilon should depend on the numbers compared!
  def fltCompare(x : Double, y : Double) = (y-flt_epsilon) < x && x < (y+flt_epsilon)

  property("empty constructor satisfies identity") = forAll { (x: Double, y: Double) =>
    val Vector2D(_x,_y) = TransformationMatrix().applyToPoint(x,y)
    all(
      ("_x = " + _x) |: (_x == x),
      ("_y = " + _y) |: (_y == y)
    )
  }

  property("translation on identity equals summation") =
    forAll { (x: Double, y: Double, xdelta: Double, ydelta: Double) =>
      val Vector2D(_x,_y) = TransformationMatrix().translate(xdelta,ydelta).applyToPoint(x,y)
      all(
        ("_x = " + _x) |: (_x == x + xdelta),
        ("_y = " + _y) |: (_y == y + ydelta)
      )
    }
}
