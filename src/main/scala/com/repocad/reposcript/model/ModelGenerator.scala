package com.repocad.reposcript.model

import java.util.concurrent.TimeUnit

import com.repocad.reposcript.Evaluator.Error
import com.repocad.reposcript.lexing.Position
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{Compiler, Evaluator, EvaluatorEnv}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
  * Generates a [[com.repocad.reposcript.model.ShapeModel]] from an AST.
  */
class ModelGenerator(parser: Parser) {

  private lazy val emptyRendererEnv: EvaluatorEnv =
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

  def eval(expr: Expr, fontMetrics: FontMetrics, env: EvaluatorEnv = EvaluatorEnv.empty): Either[String, ShapeModel] = {
    try {
      val renderer = new ModelGeneratorRenderer(fontMetrics)
      eval(expr, env ++ renderer.toEvaluatorEnv, renderer).fold(e => {
        Left(e)
      }, tuple => Right(tuple._3.model))
    } catch {
      case e: Exception =>
        Left(s"Failure when evaluating script: ${e.getLocalizedMessage}")
    }
  }

  def eval(expr: Expr, env: EvaluatorEnv, renderer: ModelGeneratorRenderer): Evaluator.Value = {
    expr match {

      case ImportExpr(name) =>
        Await.result(
          parser.remoteCache.get(name, Position.empty, code => parser.parse(code)), Duration(500, TimeUnit.MILLISECONDS)
        ).right.flatMap(state => {
          val remotePrinterEnv: EvaluatorEnv = env ++ emptyRendererEnv
          eval(state.expr, remotePrinterEnv, renderer).right.map(t => (t._1 ++ env, t._2, t._3))
        }).left.map(_.toString)

      case v: ValueExpr[_] => Right((env, v.value, renderer))

      case DefExpr(name, valExpr) =>
        eval(valExpr, env, renderer).fold(Left(_), value =>
          Right((env.add(name, Nil, valExpr.t, value._2), value._2, value._3)))

      case FunctionType(name, params, body) =>
        val function = params.size match {
          case 0 => (funEnv: EvaluatorEnv) => eval(body, funEnv, renderer).fold(l => l, r => r._2)
          case 1 => (funEnv: EvaluatorEnv, a: Any) => {
            eval(body, funEnv.add(params.head.name, Nil, params.head.t, a), renderer).fold(l => l, r => r._2)
          }
          case 2 => (funEnv: EvaluatorEnv, a: Any, b: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b), renderer
            ).fold(l => l, r => r._2)
          case 3 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c), renderer
            ).fold(l => l, r => r._2)
          case 4 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d), renderer
            ).fold(l => l, r => r._2)
          case 5 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any, e: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d)
              .add(params(4).name, Nil, params(4).t, e), renderer
            ).fold(l => l, r => r._2)
          case 6 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any, e: Any, f: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d)
              .add(params(4).name, Nil, params(4).t, e)
              .add(params(5).name, Nil, params(5).t, f), renderer
            ).fold(l => l, r => r._2)
          case x => Left("Unsupported number of arguments: " + x)
        }
        Right((env.add(name, params, body.t, function), function, renderer))

      case IfExpr(condition, ifBody, elseBody, t) =>
        eval(condition, env, renderer) match {
          case Left(thisIsBad) => Left(thisIsBad)
          case Right((conditionEnvironment, true, _)) => eval(ifBody, conditionEnvironment, renderer)
          case Right((conditionEnvironment, false, _)) => eval(elseBody, conditionEnvironment, renderer)
          case Right((newEnvironment, value, _)) => Left("Expected boolean, got " + value)
        }

      case CallExpr(name, t, params) =>
        env.get(name, params, t).fold[Evaluator.Value](
          Left(s"Failed to find a function or object called '$name'. Please ensure it has been declared.")) {
          case objectParams: Seq[String@unchecked] =>
            if (params.size != objectParams.size) {
              Left(Error.OBJECT_PARAM_SIZE_NOT_EQUAL(name, objectParams.size, params.size))
            } else {
              val defaultParameters = getDefaultParameters(t)
              val evaluatedParameters = (objectParams.zip(params).toMap ++ defaultParameters).map(t => t._1 -> eval(t._2, env, renderer))
              val lefts = evaluatedParameters.filter(t => t._2.isLeft)
              if (lefts.nonEmpty) {
                Left(Error.OBJECT_PARAM_EVAL_ERROR(name, lefts.values.toSeq))
              } else {
                Right((env, evaluatedParameters.map(t => (t._1, t._2.right.get._2)), renderer))
              }
            }

          case f: ((EvaluatorEnv@unchecked) => Any@unchecked) => Right((env, f(env), renderer))
          case f: ((EvaluatorEnv@unchecked, Any@unchecked) => Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a => Right((a._1, f.apply(env, a._2), a._3)))
          case f: ((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked) => Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                Right((b._1, f.apply(env, a._2, b._2), b._3))
              )
            )
          case f: ((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked) => Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c => {
                  Right((c._1, f.apply(env, a._2, b._2, c._2), c._3))
                }
                )
              )
            )
          case f: ((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked) =>
            Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c =>
                  eval(params(3), c._1, c._3).right.flatMap(d => {
                    Right((d._1, f.apply(env, a._2, b._2, c._2, d._2), d._3))
                  })
                )
              )
            )
          case f: ((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked,
            Any@unchecked) => Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c =>
                  eval(params(3), c._1, c._3).right.flatMap(d =>
                    eval(params(4), d._1, d._3).right.flatMap(e =>
                      Right((e._1, f.apply(env, a._2, b._2, c._2, d._2, e._2), e._3))
                    )
                  )
                )
              )
            )
          case f: ((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked,
            Any@unchecked, Any@unchecked) => Any@unchecked) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c =>
                  eval(params(3), c._1, c._3).right.flatMap(d =>
                    eval(params(4), d._1, d._3).right.flatMap(e =>
                      eval(params(5), e._1, e._3).right.flatMap(g =>
                        Right((g._1, f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2), g._3))
                      )
                    )
                  )
                )
              )
            )
          case f: (((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked,
            Any@unchecked, Any@unchecked, Any@unchecked) => Any@unchecked)) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c =>
                  eval(params(3), c._1, c._3).right.flatMap(d =>
                    eval(params(4), d._1, d._3).right.flatMap(e =>
                      eval(params(5), e._1, e._3).right.flatMap(g =>
                        eval(params(6), g._1, g._3).right.flatMap(h =>
                          Right((h._1, f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2, h._2), h._3))
                        )
                      )
                    )
                  )
                )
              )
            )
          case f: (((EvaluatorEnv@unchecked, Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked,
            Any@unchecked, Any@unchecked, Any@unchecked, Any@unchecked) => Any@unchecked)) =>
            eval(params.head, env, renderer).right.flatMap(a =>
              eval(params(1), a._1, a._3).right.flatMap(b =>
                eval(params(2), b._1, b._3).right.flatMap(c =>
                  eval(params(3), c._1, c._3).right.flatMap(d =>
                    eval(params(4), d._1, d._3).right.flatMap(e =>
                      eval(params(5), e._1, e._3).right.flatMap(g =>
                        eval(params(6), g._1, g._3).right.flatMap(h =>
                          eval(params(7), h._1, h._3).right.flatMap(i =>
                            Right((i._1, f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2, h._2, i._2), i._3))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )

          case x => Left("Expected callable function or object, got " + x)
        }

      case obj: ObjectType =>
        val paramNames = obj.params.map(_.name) ++ obj.defaultParameters
        Right((env.add(obj.name, obj.params, obj, paramNames), paramNames, renderer))

      case RefExpr(name, t) =>
        env.get(name, Nil, t).fold[Evaluator.Value](
          Left(s"Failed to find '$name' with type $t in scope. Please check if it has been declared.")
        )(rest => Right((env, rest, renderer)))

      case RefFieldExpr(refExpr, field, t) =>
        eval(refExpr, env, renderer) match {
          case Right((paramEnv, m: Map[String, Any]@unchecked, _)) => m.get(field).fold[Evaluator.Value](
            Left(s"Cannot find field '$field' in expression $refExpr")
          )(value => Right((env, value, renderer)))
          case Right(unexpected) => Left(s"Expected object for reference $refExpr, but received $unexpected")
          case Left(message) => Left(s"Could not find object to access $refExpr: $message")
        }

      case seq: BlockExpr =>
        if (seq.expr.isEmpty) {
          Right((env, Unit, renderer))
        } else {
          def foldRecursive(it: Iterator[Expr], foldEnv: EvaluatorEnv): Evaluator.Value = {
            eval(it.next(), foldEnv, renderer).fold(error => Left(error), t => {
              if (it.hasNext) {
                foldRecursive(it, t._1)
              } else {
                Right((t._1, t._2, renderer))
              }
            })
          }
          foldRecursive(seq.expr.iterator, env)
        }
      case UnitExpr => Right((env, Unit, renderer))
      case LoopExpr(loopCounterExpr: DefExpr, loopEnd: Expr, body: Expr) =>
        eval(loopCounterExpr.value, env, renderer) match {
          case Right((loopStartEnv: EvaluatorEnv, loopStartDouble: Double, _)) =>
            eval(loopEnd, env, renderer) match {
              case Right((_, loopEndDouble: Double, _)) =>
                val loopStartInt = loopStartDouble.toInt
                val loopEndInt = loopEndDouble.toInt
                /* Note to self: Too much recursion error when looping recursively */
                var loopEnv: EvaluatorEnv = loopStartEnv
                var lastResult: Any = Unit
                var lastError: Option[String] = None
                for (loopCounter <- loopStartInt until loopEndInt if lastError.isEmpty) {
                  loopEnv = loopEnv.add(loopCounterExpr.name, Nil, NumberType, loopCounter)
                  eval(body, loopEnv, renderer).fold(s => {
                    lastError = Some(s)
                    s
                  }, x => {
                    lastResult = x._2
                    loopEnv = x._1
                  })
                }
                lastError.map(Left(_)).getOrElse(Right((env, lastResult, renderer)))
              case Right((_, x, _)) => Left(Error.TYPE_MISMATCH("number", x.toString))
              case Left(x) => Left(x)
            }

          case Right((_, x, _)) => Left(Error.TYPE_MISMATCH("integer", x.toString))
          case Left(x) => Left(x)
        }

      case x => Left(s"Unknown expression $x")
    }
  }

  def getDefaultParameters(t: AnyType): Map[String, Expr] = t match {
    case ObjectType(_, _, parent, defaultParameters) => defaultParameters ++ getDefaultParameters(parent)
    case _ => Map()
  }

}
