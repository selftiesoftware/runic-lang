package com.repocad.reposcript.evaluating

import com.repocad.reposcript.lexing.{Position, Lexer}
import com.repocad.reposcript.parsing._
import com.repocad.reposcript.{Printer, _}

/**
 * An evaluator to evaluate a list of [[Expr]]
 */
class Evaluator(parser : Parser, defaultEnv : Env) {

  private val remoteCache : RemoteCache = parser.remoteCache
  
  def eval(expr : Expr, printer : Printer[_]) : Value = {
    try {
      eval(expr, defaultEnv ++ printer.toEvaluatorEnv).left.map(e => {
        println("Error when evaluating: " + e)
        e
      })
    } catch {
      case e : Exception =>
        Left(s"Failure when evaluating script: ${e.getLocalizedMessage}")
    }
  }

  def eval(expr: Expr, env : Env) : Value = {
    expr match {

      case ImportExpr(name) =>
        remoteCache.get(name, Position.empty, code => parser.parse(Lexer.lex(code))).right.flatMap(state => {
          val remotePrinterEnv : Env = env ++ Printer.emptyEvaluatorEnv
          eval(state.expr, remotePrinterEnv).right.map(t => (t._1 ++ env) -> t._2)
        }).left.map(_.toString)

      case v : ValueExpr[_] => Right(env -> v.value)

      case DefExpr(name, valExpr) =>
        eval(valExpr, env).fold(Left(_), value => Right(env.+(name -> value._2) -> value._2))

      case FunctionType(name, params, body) =>
        val function = params.size match {
          case 0 => (funEnv : Env) => eval(body, funEnv).fold(l => l, r => r._2)
          case 1 => (funEnv : Env, a: Any) => {
            eval(body, funEnv.+(params.head.name -> a)).fold(l => l, r => r._2)
          }
          case 2 => (funEnv : Env, a: Any, b: Any) =>
            eval(body, funEnv.+(params.head.name -> a, params(1).name -> b)).fold(l => l, r => r._2)
          case 3 => (funEnv : Env, a: Any, b: Any, c: Any) =>
            eval(body, funEnv.+(params.head.name -> a, params(1).name -> b, params(2).name -> c)).fold(l => l, r => r._2)
          case 4 => (funEnv : Env, a: Any, b: Any, c: Any, d: Any) =>
            eval(body, funEnv.+(params.head.name -> a, params(1).name -> b, params(2).name -> c, params(3).name -> d)).fold(l => l, r => r._2)
          case 5 => (funEnv : Env, a: Any, b: Any, c: Any, d: Any, e: Any) =>
            eval(body, funEnv.+(params.head.name -> a, params(1).name -> b, params(2).name -> c, params(3).name -> d, params(4).name -> e)).fold(l => l, r => r._2)
          case 6 => (funEnv : Env, a: Any, b: Any, c: Any, d: Any, e: Any, f: Any) =>
            eval(body, funEnv.+(params.head.name -> a, params(1).name -> b, params(2).name -> c, params(3).name -> d, params(4).name -> e, params(5).name -> f)).fold(l => l, r => r._2)
          case x => Left("Unsupported number of arguments: " + x)
        }
        Right(env.+(name -> function) -> function)

      case IfExpr(condition, ifBody, elseBody, t) =>
        eval(condition, env) match {
          case Left(thisIsBad) => Left(thisIsBad)
          case Right((conditionEnvironment, true)) => eval(ifBody, conditionEnvironment)
          case Right((conditionEnvironment, false)) => eval(elseBody, conditionEnvironment)
          case Right((newEnvironment, value)) => Left("Expected boolean, got " + value)
        }

      case CallExpr(name, t, params) =>
        env.get(name).fold[Value](Left(s"Failed to find a function or objects called '$name'. Please check if it has been declared.")) {
          case objectParams : Seq[String] =>
            if (params.size != objectParams.size) {
              Left(Error.OBJECT_PARAM_SIZE_NOT_EQUAL(name, objectParams.size, params.size))
            } else {
              val actualParams : Seq[Value] = params.map(eval(_, env))
              val (lefts, rights) = actualParams.partition((either : Value) => either.isLeft)
              if (lefts.nonEmpty) {
                Left(Error.OBJECT_PARAM_EVAL_ERROR(name, lefts))
              } else {
                val map = objectParams.zip(rights.map(_.right.get._2)).toMap
                Right(env, map)
              }
            }

          case f: ((Env) => Any)=> Right(env -> f(env))
          case f: ((Env, Any) => Any) =>
            eval(params.head, env).right.flatMap(a => Right(a._1 -> f.apply(env, a._2)))
          case f: ((Env, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                Right(b._1 -> f.apply(env, a._2, b._2))
              )
            )
          case f: ((Env, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c => {
                  Right(c._1 -> f.apply(env, a._2, b._2, c._2))}
                )
              )
            )
          case f: ((Env, Any, Any, Any, Any) => Any) =>
            eval(params.head, env).right.flatMap(a =>
              eval(params(1), a._1).right.flatMap(b =>
                eval(params(2), b._1).right.flatMap(c =>
                  eval(params(3), c._1).right.flatMap(d => {
                    Right(d._1 -> f.apply(env, a._2, b._2, c._2, d._2))
                  })
                )
              )
            )
          case f: ((Env, Any, Any, Any, Any, Any) => Any) =>
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
          case f: ((Env, Any, Any, Any, Any, Any, Any) => Any) =>
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
          case f: (((Env, Any, Any, Any, Any, Any, Any, Any) => Any)) =>
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
          case f: (((Env, Any, Any, Any, Any, Any, Any, Any, Any) => Any)) =>
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

      case ObjectType(name, params, _) =>
        val paramNames = params.map(_.name)
        Right(env.+(name -> paramNames), paramNames)

      case RefExpr(name, t) =>
        env.get(name).fold[Value](
          Left(s"Failed to find '$name' in scope. Please check if it has been declared.")
        )(rest => Right(env -> rest))

      case RefFieldExpr(refExpr, field, t) =>
        eval(refExpr, env) match {
          case Right((_, m : Map[String, Any])) => m.get(field).fold[Value](
            Left(s"Cannot find field '$field' in expression $refExpr")
          )(value => Right(env -> value))
          case Right(unexpected) => Left(s"Expected object for reference $refExpr, but received $unexpected")
          case Left(message) => Left(s"Could not find object to access $refExpr: $message")
        }

      case seq: BlockExpr =>
        if (seq.expr.isEmpty) {
          Right(env, Unit)
        } else {
          def foldRecursive(it: Iterator[Expr], foldEnv: Env): Value = {
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
      case LoopExpr(loopCounterExpr: DefExpr, loopEnd : Expr, body: Expr) =>
        eval(loopCounterExpr.value, env) match {
          case Right((loopStartEnv : Env, loopStartDouble : Double)) =>
            eval(loopEnd, env) match {
              case Right((_, loopEndDouble : Double)) =>
                val loopStartInt = loopStartDouble.toInt
                val loopEndInt = loopEndDouble.toInt
                /* Note to self: Too much recursion error when looping recursively */
                var loopEnv: Env = loopStartEnv
                var lastResult: Any = Unit
                var lastError: Option[String] = None
                for (loopCounter <- loopStartInt until loopEndInt if lastError.isEmpty) {
                  loopEnv = loopEnv.updated(loopCounterExpr.name, loopCounter)
                  eval(body, loopEnv).fold(s => {
                    lastError = Some(s); s
                  }, x => {
                    lastResult = x._2
                    loopEnv = x._1
                  })
                }
                lastError.map(Left(_)).getOrElse(Right(loopEnv.filter(t => env.contains(t._1)) -> lastResult))
              case Right((_, x)) => Left(Error.TYPE_MISMATCH("number", x.toString))
              case Left(x) => Left(x)
            }

          case Right((_, x)) => Left(Error.TYPE_MISMATCH("integer", x.toString))
          case Left(x) => Left(x)
        }

      case x => Left(s"Unknown expression $x")
    }
  }

}
