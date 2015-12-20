package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._
import com.repocad.reposcript.{HttpClient, RemoteCache}

/**
 * Parses code into drawing expressions (AST)
 */
class Parser(val httpClient : HttpClient, val defaultEnv : ParserEnv) {

  val remoteCache = new RemoteCache(httpClient)

  private val DEFAULT_LOOP_COUNTER = "counter"

  def parse(tokens : LiveStream[Token]) : Value = {
    parse(tokens, spillEnvironment = false)
  }

  def parse(tokens : LiveStream[Token], spillEnvironment : Boolean) : Value = {
    try {
      parseUntil(parse, _ => false, ParserState(UnitExpr, defaultEnv, tokens),
                 state => Right(state), e => Left(e), spillEnvironment)
    } catch {
      case e : InternalError => Left("Script too large (sorry - we're working on it!)")
      case e : Exception => Left(e.getLocalizedMessage)
    }
  }

  def parse(state : ParserState, successWithoutSuffix: SuccessCont, failure: FailureCont): Value = {
    val success : SuccessCont = prefixState => parseSuffix(prefixState, successWithoutSuffix, failure)

    state.tokens match {

      // Import
      case SymbolToken("import") :~: SymbolToken(script) :~: tail =>
        val res = remoteCache.get(script, code => parse(Lexer.lex(code), spillEnvironment = true))
        res match {
          case Left(error) => failure(error)
          case Right(importState) =>
            success(ParserState(ImportExpr(script), state.env.++(importState.env), tail))
        }

      case SymbolToken("if") :~: tail =>
        parse(state.copy(tokens = tail), conditionState => {
          if (conditionState.expr.t != BooleanType) {
            failure(Error.TYPE_MISMATCH(BooleanType.toString, conditionState.expr.t.toString))
          } else {
            parse(conditionState, ifState => {
              ifState.tokens match {
                case SymbolToken("else") :~: elseIfTail =>
                  parse(ifState.copy(tokens = elseIfTail), elseState => {
                    success(ParserState(IfExpr(conditionState.expr, ifState.expr,
                      elseState.expr, ifState.expr.t.findCommonParent(elseState.expr.t)),
                      state.env, elseState.tokens))
                  }, failure)
                case _ => success(ParserState(IfExpr(conditionState.expr, ifState.expr,
                  UnitExpr, ifState.expr.t.findCommonParent(UnitType)), state.env, ifState.tokens))
              }
            }, failure)
          }
        }, failure)

      // Loops
      case SymbolToken("repeat") :~: tail => parseLoop(state.copy(tokens = tail), success, failure)

      // Functions and objects
      case SymbolToken("def") :~: tail => parseDefinition(state.copy(tokens = tail), success, failure)

      // Blocks
      case PunctToken("{") :~: tail => parseUntil("}", state.copy(expr = UnitExpr, tokens = tail), success, failure)
      case PunctToken("(") :~: tail => parseUntil(")", state.copy(expr = UnitExpr, tokens = tail), success, failure)

      // Calls to functions or objects
      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        def parseCall(originalParameters : Seq[RefExpr], t : AnyType, errorFunction : String => String) : Value = {
          parseUntil(")", state.copy(tokens = tail), (parameterState: ParserState) => parameterState.expr match {
            case BlockExpr(params) =>
              if (verifySameParams(originalParameters, params)) {
                success(ParserState(CallExpr(name, t, params), state.env, parameterState.tokens))
              } else {
                failure(errorFunction(params.toString))
              }
            case _ => failure(Error.EXPECTED_PARAMETERS(state.expr.toString))
          }, failure)
        }

        state.env.getAsType(name, _.isInstanceOf[FunctionType]) match {
          case Some(function: FunctionExpr) =>
            parseCall(function.params, function.t.returnType, Error.EXPECTED_FUNCTION_PARAMETERS(name, function.params.toString, _))
          case None =>
            state.env.getAsType(name, _.isInstanceOf[ObjectType]) match {
              case Some(o: ObjectType) =>
                parseCall(o.params, o.t, Error.EXPECTED_OBJECT_PARAMETERS(name, o.params.toString, _))
              case None => state.env.get(name) match {
                case Some(expr) => success(ParserState(RefExpr(name, expr.t), state.env, state.tokens.tail))
                case _ => failure(Error.FUNCTION_NOT_FOUND(name))
              }
            }
        }

      // Values
      case BooleanToken(value: Boolean) :~: tail => success(ParserState(BooleanExpr(value), state.env, tail))
      case SymbolToken("false") :~: tail => success(ParserState(BooleanExpr(false), state.env, tail))
      case SymbolToken("true") :~: tail => success(ParserState(BooleanExpr(true), state.env, tail))
      case DoubleToken(value : Double) :~: tail => success(ParserState(NumberExpr(value), state.env, tail))
      case IntToken(value: Int) :~: tail => success(ParserState(NumberExpr(value), state.env, tail))
      case StringToken(value : String) :~: tail => success(ParserState(StringExpr(value), state.env, tail))

      // References to values, functions or objects
      case SymbolToken(name) :~: tail =>
        state.env.get(name) match {
          case Some(expr) => success(ParserState(RefExpr(name, expr.t), state.env, tail))
          case _ => failure(Error.REFERENCE_NOT_FOUND(name))
        }

      case rest if rest.isEmpty => success(state.copy(UnitExpr, tokens = rest))

      case xs => failure(s"Unrecognised token pattern $xs")
    }
  }

  private def parseDefinition(outerState : ParserState, success : SuccessCont, failure : FailureCont) : Value = {
    def parseFunctionParameters(parameterTokens : LiveStream[Token],
                                success : (Seq[RefExpr], LiveStream[Token]) => Value, failure : FailureCont) = {
      parseUntil(parseParameters, _.head.tag.toString == ")", ParserState(UnitExpr, outerState.env, parameterTokens),
        parametersState => { parametersState.expr match {
          case BlockExpr(exprs) if exprs.contains((e : Expr) => !e.isInstanceOf[RefExpr]) =>
            failure(Error.EXPECTED_PARAMETERS(parameterTokens.toString))
          case BlockExpr(exprs) =>
            success(exprs.asInstanceOf[Seq[RefExpr]], parametersState.tokens)
          case unknown => failure(Error.EXPECTED_PARAMETERS(unknown.toString))
        }
      }, failure)
    }

    def parseFunctionParametersAndBody(innerState : ParserState, success : (Seq[RefExpr], Expr,
                                       LiveStream[Token]) => Value, failure : FailureCont) : Value = {
      parseFunctionParameters(innerState.tokens, (parameters, parameterTokens) => {
        parameterTokens match {
          case SymbolToken("=") :~: functionTail =>
            parseFunctionBody(innerState.copy(env = innerState.env, tokens = functionTail), parameters,
              bodyState => success(parameters, bodyState.expr, bodyState.tokens), failure)
          case tail => failure(Error.SYNTAX_ERROR("=", tail.toString))
        }
      }, failure)
    }

    def parseFunctionBody(parametersState : ParserState, paramsEnv : Seq[RefExpr], success : SuccessCont,
                          failureCont: FailureCont) : Value = {
      parse(ParserState(UnitExpr, outerState.env ++ paramsEnv.map(ref => ref.name -> ref).toMap, parametersState.tokens),
        success, failure)
    }

    outerState.tokens match {
      /* Functions or objects */
      case PunctToken("(") :~: tail => parseFunctionParameters(tail, (firstParams, firstTail) => {
        firstTail match {
          case SymbolToken(name) :~: PunctToken("(") :~: functionTail =>
            parseFunctionParametersAndBody(outerState.copy(tokens = functionTail), (secondParams, body, bodyTail) => {
              val parameters = firstParams ++ secondParams
              val function = FunctionExpr(name, parameters, body)
              success(ParserState(function, outerState.env.+(name -> function), bodyTail))
            }, failure)

          case SymbolToken(name) :~: SymbolToken("=") :~: functionTail =>
            parseFunctionBody(outerState.copy(tokens = functionTail), firstParams, bodyState => {
              val function = FunctionExpr(name, firstParams, bodyState.expr)
              success(ParserState(function, outerState.env.+(name -> function), bodyState.tokens))
            }, failure)
        }
      }, failure)

      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        parseFunctionParameters(tail, (parameters, paramsTail) => {
          paramsTail match {
            case SymbolToken("=") :~: bodyTokens =>
              parseFunctionBody(outerState.copy(tokens = bodyTokens), parameters, bodyState => {
                val function = FunctionExpr(name, parameters, bodyState.expr)
                success(ParserState(function, outerState.env.+(name -> function), bodyState.tokens))
              }, failure)

            case SymbolToken("{") :~: _ => failure(Error.SYNTAX_ERROR("=", "}"))

              // Extend objects
            //case SymbolToken("as")

            case objectTail =>
              val objectExpr = ObjectType(name, parameters, AnyType)
              success(ParserState(objectExpr, outerState.env + (name -> objectExpr), objectTail))
          }
        }, failure)


      /* Assignments */
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: SymbolToken("=") :~: tail =>
        verifyType(typeName, outerState.env).right.flatMap(parentType =>
          parse(outerState.copy(tokens = tail), assignmentState => {
            val assignmentExpr = assignmentState.expr
            parentType.isChild(assignmentExpr.t) match {
              case true => success(ParserState(DefExpr(name, assignmentExpr),
                assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
              case false => failure(s"'$name' has the expected type $parentType, but was assigned to type ${assignmentExpr.t}")
            }
          }, failure)
        )

      case SymbolToken(name) :~: SymbolToken("=") :~: tail =>
        parse(outerState.copy(tokens = tail), assignmentState => {
          val assignmentExpr = assignmentState.expr
          success(ParserState(DefExpr(name, assignmentExpr),
            assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
        }, failure)
          //success(DefExpr(name, e), env + (name -> e), stream), failure)

    }
  }

  private def parseLoop(state : ParserState, success: SuccessCont, failure: String => Value) : Value = {
    def parseLoopWithRange(counterName : String, from : Expr, to : Expr, bodyTokens : LiveStream[Token], success: SuccessCont, failure: String => Value) : Value = {
      if (!NumberType.isChild(from.t)) {
        failure(Error.TYPE_MISMATCH("number", from.t.toString, "defining the number to start from in a loop"))
      } else if (!NumberType.isChild(to.t)) {
        failure(Error.TYPE_MISMATCH("number", to.t.toString, "defining when to end a loop"))
      } else {
        parse(ParserState(UnitExpr, state.env + (counterName -> from), bodyTokens), bodyState => {
          success(ParserState(LoopExpr(DefExpr(counterName, from), to, bodyState.expr),
                  state.env, bodyState.tokens))
        }, failure)
      }
    }
    def parseLoopWithCounterName(fromExpr : Expr, toExpr : Expr, loopNameTail : LiveStream[Token]) = {
      loopNameTail match {
        case SymbolToken(counterName) :~: tail => parseLoopWithRange(counterName, fromExpr, toExpr, tail, success, failure)
        case unknown => failure(Error.SYNTAX_ERROR("name of a loop variable", unknown.toString))
      }
    }
    parse(state, firstState => firstState.tokens match {
      case SymbolToken("to") :~: toTail => parse(firstState.copy(tokens = toTail), toState => toState.tokens match {
        case SymbolToken("using") :~: secondTail => parseLoopWithCounterName(firstState.expr, toState.expr, secondTail)
        case _ => parseLoopWithRange(DEFAULT_LOOP_COUNTER, firstState.expr, toState.expr, toState.tokens, success, failure)
      }, failure)
      case SymbolToken("using") :~: usingTail => parseLoopWithCounterName(NumberExpr(1), firstState.expr, usingTail)
      case _ => parseLoopWithRange(DEFAULT_LOOP_COUNTER, NumberExpr(1), firstState.expr, firstState.tokens, success, failure)
    }, failure)
  }

  private def parseParameters(state : ParserState, success : SuccessCont, failure : FailureCont) : Value = {
    state.tokens match {
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: tail =>
        verifyType(typeName, state.env) match {
          case Right(t) =>
            val reference = RefExpr(name, t)
            success(ParserState(reference, state.env.+(name -> reference), tail))
          case Left(error) => failure(error)
        }
      case SymbolToken(name) :~: tail => failure(Error.EXPECTED_TYPE_PARAMETERS(name))
    }
  }

  private def parseSuffix(state : ParserState, success : SuccessCont, failure : FailureCont) : Value = {
    state.tokens match {

      // Object field accessors
      case PunctToken(".") :~: SymbolToken(accessor) :~: tail =>
        def findParamFromObject(reference : Expr, obj : ObjectType) : Value = {
          obj.params.find(_.name == accessor).map(
            param => success(ParserState(RefFieldExpr(reference, param.name, param.t), state.env, tail))
          ).getOrElse(failure(Error.OBJECT_UNKNOWN_PARAMETER_NAME(obj.name, accessor)))
        }

        state.expr match {
          case call : CallExpr if call.t.isInstanceOf[ObjectType] => findParamFromObject(call, call.t.asInstanceOf[ObjectType])
          case ref : RefExpr if ref.t.isInstanceOf[ObjectType] => findParamFromObject(ref, ref.t.asInstanceOf[ObjectType])

          case unknown => failure(Error.EXPECTED_OBJECT_ACCESS(state.expr.toString))
        }

      case SymbolToken(name) :~: tail =>
        def parseAsFunction(f : ParserState => Value) : Value = parse(ParserState(UnitExpr, state.env, tail), f, failure)

        state.env.getAsType(name, t => t.isInstanceOf[FunctionType]) match {
          case Some(f: FunctionExpr) if f.params.size == 1 => parseAsFunction(firstState => {
            // Should be parsed as normal
            success(state)
          })
          // If the call requires two parameters, we look to the previous expression
          case Some(f: FunctionExpr) if f.params.size == 2 =>
            state.expr match {
            case firstParameter : Expr if f.params.head.t.isChild(firstParameter.t) =>
              parseAsFunction(secondState => {
                val secondParameter = secondState.expr
                if (f.params(1).t.isChild(secondParameter.t)) {
                  success(ParserState(CallExpr(name, f.t.returnType, Seq(firstParameter, secondParameter)), state.env, secondState.tokens))
                } else {
                  failure(Error.TYPE_MISMATCH(f.params.head.t.toString, firstParameter.t.toString))
                }
              })
            case _ => state.env.get (name) match {
              case Some (expr) => success (state.copy(expr = RefExpr(name, expr.t), tokens = tail) )
              case _ => failure (Error.REFERENCE_NOT_FOUND (name))
            }
          }
          case _ => success(state)
        }

      case _ => success(state)
    }
  }

  private def parseUntil(token : String, state : ParserState,
                         success : SuccessCont, failure: FailureCont): Value = {
    parseUntil(parse, stream => stream.head.toString.equals(token), state, success, failure)
  }

  private def parseUntil(parseCallback : (ParserState, SuccessCont, FailureCont) => Value,
                         condition : LiveStream[Token] => Boolean, beginningState : ParserState,
                         success : SuccessCont, failure : FailureCont, spillEnvironment : Boolean = false): Value = {
    var stateVar = beginningState
    var seqExpr : Seq[Expr] = Seq()
    var seqFail : Option[String] = None
    def seqSuccess: SuccessCont = state => {
      stateVar = state
      Right(stateVar)
    }
    def seqFailure: (String) => Value = (s) => {
      seqFail = Some(s)
      Left(s)
    }
    while (seqFail.isEmpty && !stateVar.tokens.isPlugged && !condition(stateVar.tokens)) {
      parseCallback(stateVar, seqSuccess, seqFailure) match {
        case Left(s) => seqFail = Some(s)
        case Right(newState) =>
          if (newState.expr != UnitExpr) {
            seqExpr :+= newState.expr
          }
          stateVar = newState
      }
    }
    if (!stateVar.tokens.isPlugged && condition(stateVar.tokens) ) {
      stateVar = stateVar.copy(tokens = stateVar.tokens.tail)
    }
    seqFail.map(seqFailure).getOrElse(
      if (spillEnvironment) {
        success(stateVar.copy(expr = BlockExpr(seqExpr)))
      } else {
        success(stateVar.copy(expr = BlockExpr(seqExpr), env = beginningState.env))
      }
    )
  }

  private def verifySameParams(parameters : Seq[RefExpr], callParameters : Seq[Expr]) : Boolean = {
    if (parameters.size != callParameters.size) {
      return false
    }

    for (i <- parameters.indices) {
      if(!parameters(i).t.isChild(callParameters(i).t)) {
        return false
      }
    }
    true
  }

  private def verifyType(typeName : String, env : ParserEnv) : Either[String, AnyType] = {
    env.getType(typeName) match {
      case Some(typeObject) => Right(typeObject)
      case _ => Left(Error.TYPE_NOT_FOUND(typeName))
    }
  }

}