package com.repocad.reposcript.export

import com.repocad.reposcript.model.ShapeModel

/**
  * Exports
  *
  * @tparam T The type of output this exporter exports to.
  */
trait Exporter[T] extends ((ShapeModel) => T)
