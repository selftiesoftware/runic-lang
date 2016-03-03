package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._
import com.repocad.reposcript.{HttpClient, RemoteCache}

/**
  * Parses code into drawing expressions (AST)
  */
class Parser(val httpClient: HttpClient, val defaultEnv: ParserEnv, val lexer: String => LiveStream[Token])
  extends BlockParser with DefinitionParser with ParserInterface {

  val remoteCache = new RemoteCache(httpClient)

  private val DEFAULT_LOOP_COUNTER = "counter"

  def parse(string: String, spillEnvironment: Boolean = false): Value[ExprState] = {
    parse(lexer(string), spillEnvironment)
  }

  def parse(tokens: LiveStream[Token], spillEnvironment: Boolean): Value[ExprState] = {
    try {
      val startState = ExprState(UnitExpr, defaultEnv, tokens)
      parseUntil[ExprState](startState, _ => false, accumulateExprState, parse,
        state => {
          if (spillEnvironment) {
            Right(state)
          }
          else {
            Right(state.copy(env = defaultEnv))
          }
        }, e => Left(e))
    } catch {
      case e: InternalError => Left(Error("Script too large (sorry - we're working on it!)", Position.empty))
      case e: Exception => Left(Error(e.getLocalizedMessage, Position.empty))
    }
  }

  override def parse(state: ExprState, successWithoutSuffix: SuccessCont[ExprState],
                     failure: FailureCont[ExprState]): Value[ExprState] = {
    val success: SuccessCont[ExprState] = prefixState => parseSuffix(prefixState, successWithoutSuffix, failure)
    state.tokens match {

      // Import
      case SymbolToken("import") :~: SymbolToken(script) :~: tail =>
        val res = remoteCache.get(script, state.position, code => parse(lexer(code), spillEnvironment = true))
        res match {
          case Left(error) => failure(error)
          case Right(importState) =>
            success(ExprState(ImportExpr(script), state.env.++(importState.env), tail))
        }

      case SymbolToken("if") :~: tail =>
        parse(state.copy(tokens = tail), conditionState => {
          if (conditionState.expr.t != BooleanType) {
            failure(Error.TYPE_MISMATCH(BooleanType.toString, conditionState.expr.t.toString)(state.position))
          } else {
            parse(conditionState, ifState => {
              ifState.tokens match {
                case SymbolToken("else") :~: elseIfTail =>
                  parse(ifState.copy(tokens = elseIfTail), elseState => {
                    success(ExprState(IfExpr(conditionState.expr, ifState.expr,
                      elseState.expr, ifState.expr.t.findCommonParent(elseState.expr.t)),
                      state.env, elseState.tokens))
                  }, failure)
                case _ => success(ExprState(IfExpr(conditionState.expr, ifState.expr,
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
      case PunctToken("{") :~: tail =>
        parseUntilToken[ExprState](state.copy(expr = UnitExpr, tokens = tail), "}", accumulateExprState, parse, success, failure)
      case PunctToken("(") :~: tail =>
        parseUntilToken[ExprState](state.copy(expr = UnitExpr, tokens = tail), ")", accumulateExprState, parse, success, failure)

      // Values
      case BooleanToken(value: Boolean) :~: tail => success(ExprState(BooleanExpr(value), state.env, tail))
      case SymbolToken("false") :~: tail => success(ExprState(BooleanExpr(false), state.env, tail))
      case SymbolToken("true") :~: tail => success(ExprState(BooleanExpr(true), state.env, tail))
      case DoubleToken(value: Double) :~: tail => success(ExprState(NumberExpr(value), state.env, tail))
      case IntToken(value: Int) :~: tail => success(ExprState(NumberExpr(value), state.env, tail))
      case StringToken(value: String) :~: tail => success(ExprState(StringExpr(value), state.env, tail))

      // Calls to functions or objects
      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        parseCallable(name, state.copy(expr = UnitExpr, tokens = state.tokens.tail), success, failure)

      // References to values, functions or objects
      case SymbolToken(name) :~: tail => parseReference(name, state.copy(tokens = tail), success, failure)

      case rest if rest.isEmpty => success(state.copy(UnitExpr, tokens = rest))

      case xs => failure(Error(s"Unrecognised token pattern $xs", xs.head.position))
    }
  }

  private def parseCallable(name: String, state: ExprState, success: SuccessCont[ExprState], failure: FailureCont[ExprState]): Value[ExprState] = {
    parseUntilToken[ExprState](state.copy(tokens = state.tokens.tail), ")", accumulateExprState, parse, (parameterState: ExprState) => {
      val parameters: Seq[Expr] = parameterState.expr match {
        case BlockExpr(params) => params
        case UnitExpr => Seq()
        case expr => Seq(expr)
      }
      state.env.getCallableWithParameters(name, parameters) match {
        case Right(function: FunctionType) => success(ExprState(CallExpr(name, function.returnType, parameters), state.env, parameterState.tokens))
        case Right(obj: ObjectType) => success(ExprState(CallExpr(name, obj, parameters), state.env, parameterState.tokens))
        // If a callable is not found, but a function or object of the same name exists, throw
        case Left(errorFunction) if state.env.getAsType(name, _.isInstanceOf[CallableType]).isRight =>
          failure(errorFunction(state.position))
        // If a callable is not found, it might also be a regular reference to a variable
        case Left(errorFunction) =>
          parseReference(name, state, success, failure)
      }
    }, failure)
  }

  private def parseLoop(state: ExprState, success: SuccessCont[ExprState],
                        failure: FailureCont[ExprState]): Value[ExprState] = {
    def parseLoopWithRange(counterName: String, from: Expr, to: Expr, bodyTokens: LiveStream[Token],
                           success: SuccessCont[ExprState], failure: FailureCont[ExprState]): Value[ExprState] = {
      if (!NumberType.isChild(from.t)) {
        failure(Error.TYPE_MISMATCH("number", from.t.toString, "defining the number to start from in a loop")(state.position))
      } else if (!NumberType.isChild(to.t)) {
        failure(Error.TYPE_MISMATCH("number", to.t.toString, "defining when to end a loop")(state.position))
      } else {
        parse(ExprState(UnitExpr, state.env + (counterName -> from), bodyTokens), bodyState => {
          success(ExprState(LoopExpr(DefExpr(counterName, from), to, bodyState.expr),
            state.env, bodyState.tokens))
        }, failure)
      }
    }
    def parseLoopWithCounterName(fromExpr: Expr, toExpr: Expr, loopNameTail: LiveStream[Token]) = {
      loopNameTail match {
        case SymbolToken(counterName) :~: tail => parseLoopWithRange(counterName, fromExpr, toExpr, tail, success, failure)
        case unknown => failure(Error.SYNTAX_ERROR("name of a loop variable", unknown.toString)(state.position))
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

  private def parseReference(name: String, state: ExprState,
                             success: SuccessCont[ExprState], failure: FailureCont[ExprState]): Value[ExprState] = {
    state.env.get(name) match {
      case Right(typeExpr: AnyType) => success(state.copy(expr = RefExpr(name, typeExpr)))
      case Right(expr) => success(state.copy(expr = RefExpr(name, expr.t)))
      case Left(errorFunction) => failure(errorFunction(state.position))
    }
  }

  private def parseSuffix(state: ExprState, success: SuccessCont[ExprState],
                          failure: FailureCont[ExprState]): Value[ExprState] = {
    state.tokens match {
      // Object field accessors
      case PunctToken(".") :~: (accessor: SymbolToken) :~: tail =>
        def findParamFromObject(reference: Expr, obj: ObjectType): Value[ExprState] = {
          obj.params.find(_.name == accessor.s).map(
            param => success(ExprState(RefFieldExpr(reference, param.name, param.t), state.env, tail))
          ).getOrElse(failure(Error.OBJECT_UNKNOWN_PARAMETER_NAME(obj.name, accessor.s)(accessor.position)))
        }

        state.expr match {
          case call: CallExpr if call.t.isInstanceOf[ObjectType] => findParamFromObject(call, call.t.asInstanceOf[ObjectType])
          case ref: RefExpr if ref.t.isInstanceOf[ObjectType] => findParamFromObject(ref, ref.t.asInstanceOf[ObjectType])

          case unknown => failure(Error.EXPECTED_OBJECT_ACCESS(state.expr.toString)(state.position))
        }

      case PunctToken(name) :~: tail if !"{}()".contains(name) => parseSuffixFunction(name, state.copy(tokens = tail), success, failure)
      case SymbolToken(name) :~: tail => parseSuffixFunction(name, state, success, failure)

      case _ => success(state)
    }
  }

  def parseSuffixFunction(name: String, state: ExprState, success: SuccessCont[ExprState],
                          failure: FailureCont[ExprState]): Value[ExprState] = {
    def parseAsFunction(f: SuccessCont[ExprState]): Value[ExprState] =
      parse(ExprState(UnitExpr, state.env, state.tokens.tail /* Exclude the last token (the name) */), f, _ => success(state))

    state.env.getAll(name).filter(_.isInstanceOf[FunctionType]).toSeq match {
      case Seq(f: FunctionType) if f.params.size == 1 =>
        // Should be parsed as normal
        success(state)

      // If the call requires two parameters, we look to the previous expression
      case Seq(f: FunctionType) if f.params.size == 2 =>
        state.expr match {
          case firstParameter: Expr if f.params.head.t.isChild(firstParameter.t) =>
            parseAsFunction(secondState => secondState.expr match {
              case BlockExpr(xs) => success(state)
              case secondParameter if f.params(1).t.isChild(secondParameter.t) =>
                state.expr match {
                  case DefExpr(defName, value) =>
                    success(secondState.copy(expr = DefExpr(defName, CallExpr(name, f.returnType, Seq(value, secondParameter)))))
                  case expr =>
                    success(ExprState(CallExpr(name, f.returnType, Seq(firstParameter, secondParameter)), state.env, secondState.tokens))
                }
              case expr =>
                failure(Error.TYPE_MISMATCH(f.params.head.t.toString, firstParameter.t.toString)(secondState.position))

            })
          case _ => success(state)
        }

      case _ => success(state)
    }

  }

}