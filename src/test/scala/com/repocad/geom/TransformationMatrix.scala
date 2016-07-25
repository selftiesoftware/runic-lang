package com.repocad.geom
import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Prop.{forAll, BooleanOperators, all}

object TransformationMatrixSpecification extends Properties("TransformationMatrix") {
  val flt_epsilon = 1E-3

  //Frankly this is wrong. Epsilon should depend on the numbers compared!
  def fltCompare(x : Double, y : Double) : Boolean = (y-flt_epsilon) < x && x < (y+flt_epsilon)

  def fltCompare(v1 : Vector2D, v2: Vector2D) : Boolean = fltCompare(v1.x, v2.x) && fltCompare(v1.y, v2.y)

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

  val reasonableDoubles = Gen.choose(-Math.pow(2,33) ,Math.pow(2,33))

  property("inverse matrix is the inverse element of the applyToPoint operation") =
    forAll(Gen.zip(reasonableDoubles, reasonableDoubles, reasonableDoubles, reasonableDoubles, reasonableDoubles, reasonableDoubles, reasonableDoubles, reasonableDoubles)) {
      (param : (Double,  Double, Double, Double, Double, Double, Double, Double)) =>
        val (a: Double, b: Double, c: Double, d:Double, e : Double, f : Double, px: Double, py: Double) = param
        val tm = TransformationMatrix(a,b,c,d,e,f)
        val _tm = tm.inverse
        val p = Vector2D(px,py)
        val _p = _tm.applyToPoint(tm.applyToPoint(p.x, p.y))
        _p.toString |: fltCompare(_p, p)
    }
}
