package com.repocad.reposcript.export

import com.repocad.reposcript.model.{ArcModel, LineModel, SeqModel, ShapeModel}
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object SvgExporterSpecification extends Properties("SvgExporter") {

  val svgPostfix = "</svg>"

  property("empty model") = {
    SvgExporter(SeqModel(Seq())) == """<svg width="0.0mm" height="0.0mm" viewBox="0.0 0.0 0.0 0.0"></svg>"""
  }

  property("line model") = forAll { (x1: Double, y1: Double, x2: Double, y2: Double) =>
    val model = LineModel(x1, y1, x2, y2)
    SvgExporter(model).contains(s"M$x1 $y1 L$x2 $y2 Z")
  }

  // Todo: Improve this test
  property("arc model") = {
    val arc = ArcModel(0, 0, 10, 0, 180)
    SvgExporter(arc).contains("M6.123233995736766E-16 10.0 A10.0 10.0 0 0 0 6.123233995736766E-16 -10.0") // Baaaad test
  }

  def svgPrefix(shapeModel: ShapeModel): String = {
    val viewBox = s"${shapeModel.boundary.xMin} ${shapeModel.boundary.yMin} ${shapeModel.boundary.width} ${shapeModel.boundary.height}"
    s"""<svg width="${shapeModel.boundary.width}mm" height="${shapeModel.boundary.height}mm" viewBox="$viewBox">"""
  }

}
