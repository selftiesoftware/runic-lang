package com.repocad.reposcript.export

import java.security.KeyStore.Entry.Attribute

import com.repocad.geom.Vector2D
import com.repocad.reposcript.model._

/**
  * Exports a [[ShapeModel]] to an SVG string.
  */
object SvgExporter extends Exporter[String] {

  /**
    * Transforms the given ShapeModel into an SVG string, complete with opening tags, width, height and viewbox.
    *
    * A model like {{{ LineModel(0, 0, 10, 5) }}} will result in the following SVG string:
    * {{{
    *   <svg width="10mm" height="5mm" viewBox="0 0 10 5">M0 0 L10 5 Z</svg>
    * }}}
    *
    * @param model The model to export.
    * @return A String containing a full SVG element, including .
    */
  override def apply(model: ShapeModel): String = {
    val builder = new StringBuilder()
    val viewBox = s"${model.boundary.xMin} ${model.boundary.yMin} ${model.boundary.width} ${model.boundary.height}"
    builder.append(s"""<svg width="${model.boundary.width}mm" height="${model.boundary.height}mm" viewBox="$viewBox">""")

    // Evaluate each model shape and put it in the string builder
    evaluate(model).foreach(builder.append)

    builder.append("</svg>")
    builder.mkString
  }

  private def evaluate(model: ShapeModel): Seq[String] = {
    model match {
      case SeqModel(models) => models.flatMap(evaluate)

      case a: ArcModel => Seq(toPath(arcToSvg(a)))
      case LineModel(x1, y1, x2, y2) => Seq(toPath(s"M$x1 $y1 L$x2 $y2 Z"))
    }
  }

  def arcToSvg(arc: ArcModel): String = {
    var start = polarToCartesian(arc.x, arc.y, arc.radius, arc.endAngle)
    var end = polarToCartesian(arc.x, arc.y, arc.radius, arc.startAngle)

    var largeArcFlag = if (arc.endAngle - arc.startAngle <= 180) "0" else "1"

    s"M${start.x} ${start.y} A${arc.radius} ${arc.radius} 0 $largeArcFlag 0 ${end.x} ${end.y}"
  }

  private def polarToCartesian(centerX: Double, centerY: Double, radius: Double, angleInDegrees: Double): Vector2D = {
    val angleInRadians = (angleInDegrees - 90) * Math.PI / 180.0

    Vector2D(
      centerX + (radius * Math.cos(angleInRadians)),
      centerY + (radius * Math.sin(angleInRadians))
    )
  }

  private def toPath(d: String): String = {
    s"""<path d="$d" stroke="black" stroke-width="1" fill="none"/>"""
  }

}
