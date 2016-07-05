package com.repocad.reposcript.model

import com.repocad.geom.Rectangle2D

/**
  * A model is a geometrical representation of things that can be drawn and that have an extend in space.
  */
trait ShapeModel {

  /**
    * The boundary of the shape in the coordinates of the global view.
    *
    * @return A Rectangle2D with a positive height and width.
    */
  def boundary: Rectangle2D

}

object ShapeModel {
  val empty = new ShapeModel {
    override def boundary: Rectangle2D = Rectangle2D(0, 0, 0, 0)
  }
}

case class ArcModel(x: Double, y: Double, radius: Double, startAngle: Double, endAngle: Double) extends ShapeModel {
  // TODO: Not correct
  val boundary = Rectangle2D(x - radius, y - radius, x + radius, y + radius)
}

case class BezierCurveModel(x1: Double, y1: Double,
                            x2: Double, y2: Double,
                            x3: Double, y3: Double,
                            x4: Double, y4: Double) extends ShapeModel {
  val boundary = Rectangle2D(
    math.min(x1, math.min(x2, math.min(x3, x4))),
    math.min(y1, math.min(y2, math.min(y3, y4))),
    math.max(x1, math.max(x2, math.max(x3, x4))),
    math.max(y1, math.max(y2, math.max(y3, y4)))
  )
}

case class CircleModel(x: Double, y: Double, radius: Double) extends ShapeModel {
  val boundary = Rectangle2D(x - radius, y - radius, x + radius, y + radius)
}

/**
  * A line. It's just a line.
  *
  * @param x1 The first x coordinate of the line.
  * @param y1 The first y coordinate of the line.
  * @param x2 The second x coordinate of the line.
  * @param y2 The second y coordinate of the line.
  */
case class LineModel(x1: Double, y1: Double, x2: Double, y2: Double) extends ShapeModel {
  val boundary = Rectangle2D(x1, y1, x2, y2)
}

/**
  * A sequence of [[ShapeModel]]s.
  */
case class SeqModel(models: Seq[ShapeModel]) extends ShapeModel {

  lazy val boundary = if (models.isEmpty) {
    Rectangle2D(0, 0, 0, 0)
  } else {
    models.tail.map(_.boundary).foldLeft(models.head.boundary)(_ expand _)
  }

}

case class TextModel(x: Double, y: Double, size: Double, text: String, font: String, fontMetrics: FontMetrics) extends ShapeModel {
  lazy val boundary = fontMetrics.calculateBoundary(this)
}
