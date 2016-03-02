package com.repocad.reposcript.evaluating

import com.repocad.reposcript.lexing.{Lexer, Position}
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{Renderer$, _}

/**
  * An evaluator to evaluate a list of [[Expr]]
  */
class Evaluator(parser: Parser, defaultEnv: EvaluatorEnv) {

  private val remoteCache: RemoteCache = parser.remoteCache

  def eval(expr: Expr, printer: Renderer[_]): Value = {
    try {
      eval(expr, defaultEnv ++ printer.toEvaluatorEnv).left.map(e => {
        println("Error when evaluating: " + e)
        e
      })
    } catch {
      case e: Exception =>
        Left(s"Failure when evaluating script: ${e.getLocalizedMessage}")
    }
  }

  def eval(expr: Expr, env: EvaluatorEnv): Value = {
    expr match {

      case ImportExpr(name) =>
        remoteCache.get(name, Position.empty, code => parser.parse(code)).right.flatMap(state => {
          val remotePrinterEnv: EvaluatorEnv = env ++ Renderer.emptyEvaluatorEnv
          eval(state.expr, remotePrinterEnv).right.map(t => (t._1 ++ env) -> t._2)
        }).left.map(_.toString)

      case v: ValueExpr[_] => Right(env -> v.value)

      case DefExpr(name, valExpr) =>
        eval(valExpr, env).fold(Left(_), value => Right(env.add(name, Nil, valExpr.t, value._2) -> value._2))

      case FunctionType(name, params, body) =>
        val function = params.size match {
          case 0 => (funEnv: EvaluatorEnv) => eval(body, funEnv).fold(l => l, r => r._2)
          case 1 => (funEnv: EvaluatorEnv, a: Any) => {
            eval(body, funEnv.add(params.head.name, Nil, params.head.t, a)).fold(l => l, r => r._2)
          }
          case 2 => (funEnv: EvaluatorEnv, a: Any, b: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
            ).fold(l => l, r => r._2)
          case 3 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
            ).fold(l => l, r => r._2)
          case 4 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d)
            ).fold(l => l, r => r._2)
          case 5 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any, e: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d)
              .add(params(4).name, Nil, params(4).t, e)
            ).fold(l => l, r => r._2)
          case 6 => (funEnv: EvaluatorEnv, a: Any, b: Any, c: Any, d: Any, e: Any, f: Any) =>
            eval(body, funEnv
              .add(params.head.name, Nil, params.head.t, a)
              .add(params(1).name, Nil, params(1).t, b)
              .add(params(2).name, Nil, params(2).t, c)
              .add(params(3).name, Nil, params(3).t, d)
              .add(params(4).name, Nil, params(4).t, e)
              .add(params(5).name, Nil, params(5).t, f)
            ).fold(l => l, r => r._2)
          case x => Left("Unsupported number of arguments: " + x)
        }
        Right(env.add(name, params, body.t, function) -> function)

      case IfExpr(condition, ifBody, elseBody, t) =>
        eval(condition, env) match {
          case Left(thisIsBad) => Left(thisIsBad)
          case Right((conditionEnvironment, true)) => eval(ifBody, conditionEnvironment)
          case Right((conditionEnvironment, false)) => eval(elseBody, conditionEnvironment)
          case Right((newEnvironment, value)) => Left("Expected boolean, got " + value)
        }

      case CallExpr(name, t, params) =>
        env.get(name, params, t).fold[Value](
          Left(s"Failed to find a function or object called '$name'. Please ensure it has been declared.")) {
          case objectParams: Seq[String] =>
            if (params.size != objectParams.size) {
              Left(Error.OBJECT_PARAM_SIZE_NOT_EQUAL(name, objectParams.size, params.size))
            } else {
              val defaultParameters = getDefaultParameters(t)
              val evaluatedParameters = (objectParams.zip(params).toMap ++ defaultParameters).map(t => t._1 -> eval(t._2, env))
              val lefts = evaluatedParameters.filter(t => t._2.isLeft)
              if (lefts.nonEmpty) {
                Left(Error.OBJECT_PARAM_EVAL_ERROR(name, lefts.values.toSeq))
              } else {
                Right(env, evaluatedParameters.map(t => t._1 -> t._2.right.get._2))
              }
            }

          case f: ((EvaluatorEnv) => Any) => Right(env -> f(env))
          case f: ((EvaluatorEnv, Any) => Any) =>
            eval(params.head, env).right.flatMap(a => Right(a._1 -> f.apply(env, a._2)))
          case f: ((EvaluatorEnv, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                Right(b._1 -> f.apply(env, a._2, b._2))
              )
            )
          case f: ((EvaluatorEnv, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c => {
                  Right(c._1 -> f.apply(env, a._2, b._2, c._2))
                }
                )
              )
            )
          case f: ((EvaluatorEnv, Any, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d => {
                    Right(d._1 -> f.apply(env, a._2, b._2, c._2, d._2))
                  })
                )
              )
            )
          case f: ((EvaluatorEnv, Any, Any, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d =>
                    eval(params(4), d._1).right.flatMap(e =>
                      Right(e._1 -> f.apply(env, a._2, b._2, c._2, d._2, e._2))
                    )
                  )
                )
              )
            )
          case f: ((EvaluatorEnv, Any, Any, Any, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d =>
                    eval(params(4), d._1).right.flatMap(e =>
                      eval(params(5), e._1).right.flatMap(g =>
                        Right(g._1 -> f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2))
                      )
                    )
                  )
                )
              )
            )
          case f: (((EvaluatorEnv, Any, Any, Any, Any, Any, Any, Any) => Any)) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d =>
                    eval(params(4), d._1).right.flatMap(e =>
                      eval(params(5), e._1).right.flatMap(g =>
                        eval(params(6), g._1).right.flatMap(h =>
                          Right(h._1 -> f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2, h._2))
                        )
                      )
                    )
                  )
                )
              )
            )
          case f: (((EvaluatorEnv, Any, Any, Any, Any, Any, Any, Any, Any) => Any)) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d =>
                    eval(params(4), d._1).right.flatMap(e =>
                      eval(params(5), e._1).right.flatMap(g =>
                        eval(params(6), g._1).right.flatMap(h =>
                          eval(params(7), h._1).right.flatMap(i =>
                            Right(i._1 -> f.apply(env, a._2, b._2, c._2, d._2, e._2, g._2, h._2, i._2))
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
        Right(env.add(obj.name, obj.params, obj, paramNames), paramNames)

      case RefExpr(name, t) =>
        env.get(name, Nil, t).fold[Value](
          Left(s"Failed to find '$name' with type $t in scope. Please check if it has been declared.")
        )(rest => Right(env -> rest))

      case RefFieldExpr(refExpr, field, t) =>
        eval(refExpr, env) match {
          case Right((paramEnv, m: Map[String, Any])) => m.get(field).fold[Value](
            Left(s"Cannot find field '$field' in expression $refExpr")
          )(value => Right(env -> value))
          case Right(unexpected) => Left(s"Expected object for reference $refExpr, but received $unexpected")
          case Left(message) => Left(s"Could not find object to access $refExpr: $message")
        }

      case seq: BlockExpr =>
        if (seq.expr.isEmpty) {
          Right(env, Unit)
        } else {
          def foldRecursive(it: Iterator[Expr], foldEnv: EvaluatorEnv): Value = {
            eval(it.next(), foldEnv).fold(error => Left(error), t => {
              if (it.hasNext) {
                foldRecursive(it, t._1)
              } else {
                Right(t._1 -> t._2)
              }
            })
          }
          foldRecursive(seq.expr.iterator, env)
        }
      case UnitExpr => Right(env -> Unit)
      case LoopExpr(loopCounterExpr: DefExpr, loopEnd: Expr, body: Expr) =>
        eval(loopCounterExpr.value, env) match {
          case Right((loopStartEnv: EvaluatorEnv, loopStartDouble: Double)) =>
            eval(loopEnd, env) match {
              case Right((_, loopEndDouble: Double)) =>
                val loopStartInt = loopStartDouble.toInt
                val loopEndInt = loopEndDouble.toInt
                /* Note to self: Too much recursion error when looping recursively */
                var loopEnv: EvaluatorEnv = loopStartEnv
                var lastResult: Any = Unit
                var lastError: Option[String] = None
                for (loopCounter <- loopStartInt until loopEndInt if lastError.isEmpty) {
                  loopEnv = loopEnv.add(loopCounterExpr.name, Nil, NumberType, loopCounter)
                  eval(body, loopEnv).fold(s => {
                    lastError = Some(s)
                    s
                  }, x => {
                    lastResult = x._2
                    loopEnv = x._1
                  })
                }
                lastError.map(Left(_)).getOrElse(Right(env -> lastResult))
              case Right((_, x)) => Left(Error.TYPE_MISMATCH("number", x.toString))
              case Left(x) => Left(x)
            }

          case Right((_, x)) => Left(Error.TYPE_MISMATCH("integer", x.toString))
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
