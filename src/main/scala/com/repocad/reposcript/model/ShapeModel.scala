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
