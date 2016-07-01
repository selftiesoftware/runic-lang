package com.repocad.reposcript.evaluating

import com.repocad.reposcript.Renderer
import com.repocad.reposcript.model.{CircleModel, SeqModel, LineModel, ShapeModel}

class ModelGeneratorRenderer extends Renderer {

  var model: SeqModel = SeqModel(Seq())

  override def arc(x: Double, y: Double, r: Double, sAngle: Double, eAngle: Double): Unit =
    ()
  override def text(x: Double, y: Double, h: Double, t: Any): Map[String, Any] =
    Map()
  override def text(x: Double, y: Double, h: Double, t: Any, font: String): Map[String, Any] =
    Map()
  override def circle(x: Double, y: Double, r: Double): Unit = {println("HERE!")
    model = model.copy(model.models :+ CircleModel(x, y, r))}
  override def line(x1: Double, y1: Double, x2: Double, y2: Double): Unit =
    model = model.copy(model.models :+ LineModel(x1, y1, x2, y2))
  override def bezierCurve(x1: Double, y1: Double, x2: Double, y2: Double, x3: Double, y3: Double, x4: Double, y4: Double): Unit =
    ()
}
