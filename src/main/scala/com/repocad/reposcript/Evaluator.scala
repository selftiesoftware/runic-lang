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

  lazy val defaultEnv: EvaluatorEnv = new EvaluatorEnv(
    Environment.primitiveEnv.map(t => {
      val functionType = t._2._1.asInstanceOf[FunctionType]
      t._1 -> Map(Signature(functionType.params.map(_.t), functionType.returnType) -> t._2._2)
    })
  )

  /**
    * Evaluates the given expression by generating a model and evaluating it on the given renderer.
    *
    * @param expr        The AST to evaluate.
    * @param parser      The parser to use when evaluating import statements.
    * @param renderer    The renderer to render the output.
    * @param fontMetrics Metrics for fonts.
    * @param environment The environment to use when evaluating the AST.
    * @return Either an error or nothing.
    */
  def eval(expr: Expr, parser: Parser, renderer: Renderer, fontMetrics: FontMetrics,
           environment: EvaluatorEnv = defaultEnv): Either[String, Unit] = {
    model(expr, parser, fontMetrics, environment).right.map(model => render(model, renderer))
  }

  /**
    * Models the given expression into a [[ShapeModel]].
    *
    * @param expr        The expression to model.
    * @param parser      The parser to use when evaluating import statements.
    * @param fontMetrics The metrics to help calculate font boundaries.
    * @param environment The environtment to use when evaluating the AST.
    * @return Either an error or a [[ShapeModel]].
    */
  def model(expr: Expr, parser: Parser, fontMetrics: FontMetrics,
            environment: EvaluatorEnv = defaultEnv): Either[String, ShapeModel] = {
    new ModelGenerator(parser).eval(expr, fontMetrics, environment)
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