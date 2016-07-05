package com.repocad.reposcript.evaluating

import com.repocad.geom.Rectangle2D
import com.repocad.reposcript.Renderer
import com.repocad.reposcript.model._

class ModelGeneratorRenderer(fontMetrics: FontMetrics) extends Renderer {

  var model: SeqModel = SeqModel(Seq())

  private def add(newModel: ShapeModel): ShapeModel = {
    model = model.copy(model.models :+ newModel)
    newModel
  }

  private def boundaryToMap(boundary: Rectangle2D): Map[String, Any] = {
    Map("x" -> boundary.width, "y" -> boundary.height)
  }

  override def arc(x: Double, y: Double, r: Double, sAngle: Double, eAngle: Double): Unit =
    add(ArcModel(x, y, r, sAngle, eAngle))

  override def bezierCurve(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double,
                           x4: Double, y4: Double): Unit = BezierCurveModel(x1, y1, x2, y2, x3, y3, x4, y4)

  override def text(x: Double, y: Double, h: Double, t: Any): Map[String, Any] =
    boundaryToMap(add(TextModel(x, y, h, t.toString, fontMetrics.defaultFont, fontMetrics)).boundary)

  override def text(x: Double, y: Double, h: Double, t: Any, font: String): Map[String, Any] =
    boundaryToMap(add(TextModel(x, y, h, t.toString, font, fontMetrics)).boundary)

  override def circle(x: Double, y: Double, r: Double): Unit = {
    add(CircleModel(x, y, r))
  }

  override def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    add(LineModel(x1, y1, x2, y2))

}
