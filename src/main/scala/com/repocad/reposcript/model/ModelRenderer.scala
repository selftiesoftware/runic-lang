package com.repocad.reposcript.model

import com.repocad.reposcript.Renderer

/**
  * Evaluates a [[ShapeModel]] via a [[com.repocad.reposcript.Renderer]]
  */
object ModelRenderer {

  /**
    * Renders the given model with the given renderer.
    *
    * @param model    The model to render.
    * @param renderer The renderer to render upon.
    */
  def render(model: ShapeModel, renderer: Renderer): Unit = {
    model match {
      case ArcModel(x, y, radius, startAngle, endAngle, _) => renderer.arc(x, y, radius, startAngle, endAngle)
      case BezierCurveModel(x1, y1, x2, y2, x3, y3, x4, y4, _) => renderer.bezierCurve(x1, y1, x2, y2, x3, y3, x4, y4)
      case CircleModel(x, y, radius, _) => renderer.circle(x, y, radius)
      case LineModel(x1, y1, x2, y2, _) => renderer.line(x1, y1, x2, y2)
      case SeqModel(models, _) => models.foreach(newModel => render(newModel, renderer))
      case TextModel(x, y, size, text, font, _, _) => renderer.text(x, y, size, text, font)
      case e => // Do nothing
    }
  }

}
