package com.repocad.reposcript

import com.repocad.reposcript.model._
import com.repocad.reposcript.parsing._

/**
  * An evaluator to evaluate a list of [[Expr]] on a given renderer. The evaluator works in two steps:
  * 1) Model the expression into a [[ShapeModel]]
  * 2) Evaluate the [[ShapeModel]] on a [[Renderer]]
  */
object Evaluator {

  type Value = Either[String, (EvaluatorEnv, Any, ModelGeneratorRenderer)]

  object Error {
    def OPERATOR_NOT_FOUND(x: String): String = s"Failed to find operator '$x'. Has it been defined?"

    def OBJECT_PARAM_EVAL_ERROR(name: String, lefts: Seq[Value]): String =
      s"Failed to evaluate ${lefts.size} parameters when creating object '$name': $lefts"

    def OBJECT_PARAM_SIZE_NOT_EQUAL(objectName: String, expectedParams: Int, actualParams: Int) =
      s"Object '$objectName' requires $expectedParams parameters, but was given $actualParams"

    def TYPE_MISMATCH(expected: String, actual: String) = s"Expected type $expected but found $actual"
  }

  lazy val emptyEnv: EvaluatorEnv =
    EvaluatorEnv()
      .add("arc", getNumberTypeReferences("x", "y", "r", "sAngle", "eAngle"), UnitType,
        (env: EvaluatorEnv, x: Double, y: Double, r: Double, sAngle: Double, eAngle: Double) => Unit)
      .add("bezier", getNumberTypeReferences("x1", "y2", "x2", "y2", "x3", "y3", "x4", "y4"), UnitType,
        (env: EvaluatorEnv, x1: Double, y1: Double, x2: Double, y2: Double, x3: Double,
         y3: Double, x4: Double, y4: Double) => Unit)
      .add("circle", getNumberTypeReferences("x", "y", "r"), UnitType,
        (env: EvaluatorEnv, x: Double, y: Double, r: Double) => Unit)
      .add("line", getNumberTypeReferences("x1", "y1", "x2", "y2"), UnitType,
        (env: EvaluatorEnv, x1: Double, y1: Double, x2: Double, y2: Double) => Unit)
      .add("text", getNumberTypeReferences("x", "y", "h") :+ RefExpr("t", AnyType), Compiler.vectorType,
        (env: EvaluatorEnv, x: Double, y: Double, h: Double, t: Any) => Unit)
      .add("text", getNumberTypeReferences("x", "y", "h").:+(RefExpr("t", AnyType)).:+(RefExpr("font", StringType)), Compiler.vectorType,
        (env: EvaluatorEnv, x: Double, y: Double, h: Double, t: Any, font: String) => Unit)

  def getNumberTypeReferences(names: String*): Seq[RefExpr] = {
    for (name <- names) yield RefExpr(name, NumberType)
  }

  /**
    * Evaluates the given expression by generating a model and evaluating it on the given renderer.
    *
    * @param expr        The AST to evaluate.
    * @param parser      The parser to use when evaluating import statements.
    * @param environment The environment to use when evaluating the AST.
    * @param renderer    The renderer to render the output.
    * @param fontMetrics Metrics for fonts.
    * @return Either an error or nothing.
    */
  def eval(expr: Expr, parser: Parser, environment: EvaluatorEnv, renderer: Renderer, fontMetrics: FontMetrics): Either[String, Unit] = {
    model(expr, parser, environment, fontMetrics).right.map(model => render(model, renderer))
  }

  /**
    * Models the given expression into a [[ShapeModel]].
    *
    * @param expr        The expression to model.
    * @param parser      The parser to use when evaluating import statements.
    * @param environment The environtment to use when evaluating the AST.
    * @param fontMetrics The metrics to help calculate font boundaries.
    * @return Either an error or a [[ShapeModel]].
    */
  def model(expr: Expr, parser: Parser, environment: EvaluatorEnv, fontMetrics: FontMetrics): Either[String, ShapeModel] = {
    new ModelGenerator(parser).eval(expr, environment, fontMetrics)
  }

  /**
    * Renders the model on a renderer.
    *
    * @param model    The model to render.
    * @param renderer A renderer with information about how to render.
    */
  def render(model: ShapeModel, renderer: Renderer): Unit = {
    ModelRenderer.render(model, renderer)
  }

}