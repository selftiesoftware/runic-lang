package com.repocad.reposcript.model

import com.repocad.geom.Rectangle2D
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{EvaluatorEnv, Renderer}

class ModelGeneratorRenderer(fontMetrics: FontMetrics) extends Renderer {

  var model: SeqModel = SeqModel(Seq())

  private val vectorType = ObjectType("vector", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType)), AnyType)

  val toEvaluatorEnv: EvaluatorEnv =
    EvaluatorEnv()
      .add("arc", getNumberTypeReferences("x", "y", "r", "sAngle", "eAngle"), UnitType,
        (env: EvaluatorEnv, x: Double, y: Double, r: Double, sAngle: Double, eAngle: Double) =>
          arc(x, y, r, sAngle, eAngle))
      .add("bezier", getNumberTypeReferences("x1", "y2", "x2", "y2", "x3", "y3", "x4", "y4"), UnitType,
        (env: EvaluatorEnv, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double,
         y3: Double, x4: Double, y4: Double) => bezierCurve(x1, y1, x2, y2, x3, y3, x4, y4))
      .add("circle", getNumberTypeReferences("x", "y", "r"), UnitType,
        (env: EvaluatorEnv, x: Double, y: Double, r: Double) => circle(x, y, r))
      .add("line", getNumberTypeReferences("x1", "y1", "x2", "y2"), UnitType,
        (env: EvaluatorEnv, x1: Double, y1: Double, x2: Double, y2: Double) => line(x1, y1, x2, y2))
      .add("text", getNumberTypeReferences("x", "y", "h") :+ RefExpr("t", AnyType), vectorType,
        (env: EvaluatorEnv, x: Double, y: Double, h: Double, t: Any) => text(x, y, h, t))
      .add("text", getNumberTypeReferences("x", "y", "h").:+(RefExpr("t", AnyType)).:+(RefExpr("font", StringType)), vectorType,
        (env: EvaluatorEnv, x: Double, y: Double, h: Double, t: Any, f: String) => text(x, y, h, t, f))
      .add("vector", Seq(RefExpr("x", NumberType), RefExpr("y", NumberType)), vectorType,
        (env: EvaluatorEnv, x: Double, y: Double) => Map("x" -> x, "y" -> y))

  private def getNumberTypeReferences(names: String*): Seq[RefExpr] = {
    for (name <- names) yield RefExpr(name, NumberType)
  }

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
