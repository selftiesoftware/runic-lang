package com.repocad.reposcript.evaluating

import com.repocad.reposcript._
import com.repocad.reposcript.model.ModelRenderer
import com.repocad.reposcript.parsing._

/**
  * An evaluator to evaluate a list of [[Expr]] on a given renderer.
  */
class Evaluator(parser: Parser, defaultEnv: EvaluatorEnv) {

  private val remoteCache: RemoteCache = parser.remoteCache

  def eval(expr: Expr, renderer: Renderer): Unit = {
    new ModelGenerator(parser)
      .eval(expr, defaultEnv)
      .fold(println, model => ModelRenderer.render(model, renderer))
  }

}
