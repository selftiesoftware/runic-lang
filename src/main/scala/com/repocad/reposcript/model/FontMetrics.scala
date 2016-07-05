package com.repocad.reposcript.model

import com.repocad.geom.Rectangle2D

/**
  * Calculates font metrics to be used in [[ShapeModel]]s.
  */
trait FontMetrics {

  def calculateBoundary(textModel: TextModel): Rectangle2D

  def defaultFont: String

}
