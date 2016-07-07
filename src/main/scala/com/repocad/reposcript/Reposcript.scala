package com.repocad.reposcript

import com.repocad.remote.HttpClient
import com.repocad.reposcript.model.{FontMetrics, ModelGeneratorRenderer, ShapeModel}
import com.repocad.reposcript.parsing.{Expr, Parser}

/**
  * Main API entrypoint for Reposcript.
  *
  * <p>
  * This object serves as a helper for the entire Reposcript library, and exposes lexing, parsing, modelling and
  * evaluating in simple methods with sensible defaults. If you wish to change the defaults, please consult the below
  * descriptions for each step. Otherwise, the most simple use cases are illustrated here:
  * </p>
  * <h2>Simple use cases</h2>
  * <h4>Evaluating text on a medium</h4>
  * {{{
  * val code: String = ???                     // Code to be executed
  * val httpClient: HttpClient = ???           // Client that can fetch remote scripts from a repository
  * val fontMetrics: FontMetrics               // The font metrics who knows how to calculate font boundaries
  * val renderer: Renderer = ???               // A renderer, that can render shapes on a medium
  * Reposcript.run(code, httpClient, renderer) // : Either[String, Unit]]
  * }}}
  * <h4>Producing an abstract syntax tree (AST) from text</h4>
  * {{{
  *   val code: String = ???               // Code to be parsed
  *   val httpClient: HttpClient = ???     // Client that can fetch remote scripts from a repository
  *   Reposcript.compile(code, httpClient) // : Either[String, Expr]]
  * }}}
  * <h4>Producing a [[com.repocad.reposcript.model.ShapeModel]] from an AST</h4>
  * {{{
  *   val ast: Expr = ???                // The AST to model
  *   val httpClient: HttpClient         // A client that can fetch remote scripts from a repository
  *   val fontMetrics: FontMetrics       // The font metrics who knows how to calculate font boundaries
  *   Reposcript.model(ast, fontMetrics) // : Either[String, ShapeModel]]
  * }}}
  * <h4>Rendering a model</h4>
  * {{{
  *   val model: ShapeModel              // The model to render
  *   val renderer: Renderer             // A renderer, that can render shapes on a medium
  *   Reposcript.render(model, renderer) // : Unit
  * }}}
  * <h2>Reposcript stages</h2>
  * <p>
  * If you wish to use Reposcript in a more granular way, you should know what stages Reposcript uses to do the above.
  * Reposcript first lexes the string into tokens, then parses those tokens into an abstract syntax tree (AST, typed
  * as the composite type [[Expr]]). These two steps is equivalent to the ``compile`` step. Later, the AST is evaluated
  * to a [[ShapeModel]] to describe the extend in euclidian space, and finally ``rendered`` with a [[Renderer]] on a
  * medium. This last phase is dubbed the ``evaluate`` phase.
  * </p>
  */
object Reposcript {

  /**
    * Compiles code into an abstract syntax tree (AST).
    *
    * @param code       The code to compile into expression(s).
    * @param httpClient The client used to fetch remote scripts.
    * @return Either an error or an AST.
    */
  def compile(code: String, httpClient: HttpClient): Either[String, Expr] = {
    Compiler.parse(code, httpClient).left.map(_.toString)
  }

  /**
    * Model an abstract syntax tree (AST) into a spatial representation of [[ShapeModel]].
    *
    * @param ast         The AST to model.
    * @param httpClient  A client to fetch remote scripts.
    * @param fontMetrics The metrics used to calculate font boundaries.
    * @return Either an error or a [[ShapeModel]].
    */
  def model(ast: Expr, httpClient: HttpClient, fontMetrics: FontMetrics): Either[String, ShapeModel] = {
    val parser = new Parser(httpClient)
    val modelRenderer = new ModelGeneratorRenderer(fontMetrics)
    Evaluator.model(ast, parser, fontMetrics, Evaluator.defaultEnv)
  }

  /**
    * Renders a model with the given [[Renderer]].
    *
    * @param model    The model to render.
    * @param renderer The renderer who knows how to render models.
    */
  def render(model: ShapeModel, renderer: Renderer): Unit = {
    Evaluator.render(model, renderer)
  }

  /**
    * Runs the given code through the entire Reposcript stack and render the result on the given renderer.
    *
    * @param code        The Reposcript code to render.
    * @param httpClient  A client to pull in remote scripts.
    * @param fontMetrics Fonts metrics to calculate font boundaries.
    * @param renderer    A renderer to render the code on.
    * @return Either an error or nothing (success).
    */
  def run(code: String, httpClient: HttpClient, fontMetrics: FontMetrics, renderer: Renderer): Either[String, Unit] = {
    val parser = new Parser(httpClient)
    parser.parse(code)
      .right.flatMap(state => model(state.expr, httpClient, fontMetrics))
      .right.map(model => render(model, renderer))
      .left.map(_.toString)
  }

}
