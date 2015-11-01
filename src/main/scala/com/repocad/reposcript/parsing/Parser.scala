package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._
import com.repocad.reposcript.{HttpClient, RemoteCache}

/**
 * Parses code into drawing expressions (AST)
 */
class Parser(val httpClient : HttpClient, val defaultEnv : ParserEnv) {

  val remoteCache = new RemoteCache(httpClient)

  private val DEFAULT_LOOP_COUNTER = "_loopCounter"

  def parse(tokens : LiveStream[Token]) : Value = {
    parse(tokens, spillEnvironment = false)
  }

  def parse(tokens : LiveStream[Token], spillEnvironment : Boolean) : Value = {
    try {
      parseUntil(parse, tokens, _ => false, defaultEnv, (expr, values, _) => Right((expr, values)), e => Left(e), spillEnvironment)
    } catch {
      case e : InternalError => Left("Script too large (sorry - we're working on it!)")
      case e : Exception => Left(e.getLocalizedMessage)
    }
  }

  def parse(tokens: LiveStream[Token], env : ParserEnv, success: SuccessCont, failure: FailureCont): Value = {
    tokens match {

      // Import
      case SymbolToken("import") :~: SymbolToken(script) :~: tail =>
        val res = remoteCache.get(script, code => parse(Lexer.lex(code), spillEnvironment = true))
        res match {
          case Left(error) => failure(error)
          case Right((expr, importEnv)) =>
            success(ImportExpr(script), env ++ importEnv, tail)
        }

      case SymbolToken("if") :~: tail =>
        parse(tail, env, (condition, _, conditionTail) => {
          if (condition.t != BooleanType) {
            failure(Error.TYPE_MISMATCH(BooleanType.toString, condition.t.toString))
          } else {
            parse(conditionTail, env, (ifBody, _, ifBodyTail) => {
              ifBodyTail match {
                case SymbolToken("else") :~: elseIfTail =>
                  parse(elseIfTail, env, (elseBody, _, elseBodyTail) => {
                    success(IfExpr(condition, ifBody, elseBody, ifBody.t.findCommonParent(elseBody.t)),
                      env, elseBodyTail)
                  }, failure)
                case _ => success(IfExpr(condition, ifBody, UnitExpr, ifBody.t.findCommonParent(UnitType)),
                  env, ifBodyTail)
              }
            }, failure)
          }
        }, failure)

      // Loops
      case SymbolToken("repeat") :~: tail => parseLoop(tail, env, success, failure)

      // Functions and objects
      case SymbolToken("def") :~: tail => parseDefinition(tail, env, success, failure)

      // Blocks
      case PunctToken("{") :~: tail => parseUntil(tail, PunctToken("}"), env, success, failure)
      case PunctToken("(") :~: tail => parseUntil(tail, PunctToken(")"), env, (expr, newValueEnv, exprTail) => {
        exprTail match {
          case SymbolToken(functionName) :~: functionTail if env.get(functionName).filter(_.isInstanceOf[FunctionExpr])
              .exists(_.asInstanceOf[FunctionExpr].params.size == 2) =>
            val funExpr = env.get(functionName).get.asInstanceOf[FunctionExpr]
            parseBackwardsReference(expr, funExpr, functionTail, env, success, failure)
          case _ => success(expr, newValueEnv, exprTail)
        }
      }, failure)

      // References to Functions
      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        env.get(name) match {
          case Some(function : FunctionExpr) =>
            parseUntil(tail, PunctToken(")"), env, (params : Expr, newEnv : ParserEnv, paramsTail : LiveStream[Token]) => {
              params match {
                case BlockExpr(xs : Seq[RefExpr]) =>
                  verifySimilarTypes(name, function.params, xs, newEnv).map(failure)
                    .getOrElse(success(CallExpr(name, function.body.t, xs), newEnv, paramsTail))
                case xs => failure("Expected parameters for function call. Got " + xs)
              }
            }, failure)
          case _ => failure(Error.FUNCTION_NOT_FOUND(name))
        }

      // References to Operations
      case (firstToken : Token) :~: SymbolToken(functionName) :~: tail
        if env.get(functionName).filter(_.isInstanceOf[FunctionExpr])
          .exists(_.asInstanceOf[FunctionExpr].params.size == 2) =>
        val funExpr = env.get(functionName).get.asInstanceOf[FunctionExpr]
        parse(LiveStream(Iterable(firstToken)), env, (firstExpr, _, _) => {
          parseBackwardsReference(firstExpr, funExpr, tail, env, success, failure)
        }, failure)

      // Values
      case BooleanToken(value: Boolean) :~: tail => success(BooleanExpr(value), env, tail)
      case SymbolToken("false") :~: tail => success(BooleanExpr(false), env, tail)
      case SymbolToken("true") :~: tail => success(BooleanExpr(true), env, tail)
      case DoubleToken(value : Double) :~: tail => success(NumberExpr(value), env, tail)
      case IntToken(value: Int) :~: tail => success(NumberExpr(value), env, tail)
      case StringToken(value : String) :~: tail => success(StringExpr(value), env, tail)

      case SymbolToken(name) :~: tail =>
        env.get(name) match {
          case Some(expr) => success(RefExpr(name, expr.t), env, tail)
          case _ => failure(Error.REFERENCE_NOT_FOUND(name))
        }

      case stream if stream.isEmpty => success(UnitExpr, env, stream)

      case xs => failure(s"Unrecognised token pattern $xs")
    }
  }

  private def parseBackwardsReference(firstExpr : Expr, funExpr : FunctionExpr, tokens : LiveStream[Token],
                                      env : ParserEnv, success : SuccessCont, failure : FailureCont) : Value = {
    parse(tokens, env, (secondExpr, _, secondTail) => {
      verifySimilarTypes(funExpr.name, funExpr.params, Seq(firstExpr, secondExpr), env).map(failure)
        .getOrElse(success(CallExpr(funExpr.name, funExpr.t, Seq(firstExpr, secondExpr)), env, secondTail))
    }, failure)
  }

  private def parseDefinition(tokens : LiveStream[Token], env : ParserEnv, success : SuccessCont, failure : FailureCont) : Value = {
    def parseFunctionParameters(parameterTokens : LiveStream[Token], success : (Seq[RefExpr], LiveStream[Token]) => Value, failure : FailureCont) = {
      parseUntil(parseParameters, parameterTokens, _.head.tag.toString.equals(")"), env, (params, _, paramsTail) => {
        params match {
          case BlockExpr(exprs) => success(exprs.asInstanceOf[Seq[RefExpr]], paramsTail)
          case _ => failure(Error.EXPECTED_PARAMETERS(params.toString))
        }
      }, failure)
    }

    def parseFunctionParametersAndBody(parameterTokens : LiveStream[Token], paramsEnv : Seq[RefExpr], success : (Seq[RefExpr], Expr, LiveStream[Token]) => Value, failure : FailureCont) : Value = {
      parseFunctionParameters(parameterTokens, (params, paramsTail) => {
        paramsTail match {
          case SymbolToken("=") :~: functionTail =>
            parseFunctionBody(functionTail, paramsEnv ++ params, (body, _, bodyTail) => success(params, body, bodyTail), failure)

          case tail => failure(Error.SYNTAX_ERROR("=", tail.toString))
        }
      }, failure)
    }

    def parseFunctionBody(bodyTokens : LiveStream[Token], paramsEnv : Seq[RefExpr], success : SuccessCont, failureCont: FailureCont) : Value = {
      parse(bodyTokens, env ++ paramsEnv.map(ref => ref.name -> ref).toMap, success, failure)
    }

    tokens match {
      /* Functions */
      case PunctToken("(") :~: tail => parseFunctionParameters(tail, (firstParams, firstTail) => {
        firstTail match {
          case SymbolToken(name) :~: PunctToken("(") :~: functionTail =>
            parseFunctionParametersAndBody(functionTail, firstParams, (secondParams, body, bodyTail) => {
              val function = FunctionExpr(name, firstParams ++ secondParams, body)
              success(function, env.+(name -> function), bodyTail)
            }, failure)

          case SymbolToken(name) :~: SymbolToken("=") :~: functionTail =>
            parseFunctionBody(functionTail, firstParams, (body, _, bodyTail) => {
              val function = FunctionExpr(name, firstParams, body)
              success(function, env.+(name -> function), bodyTail)
            }, failure)

        }
      }, failure)

      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        parseFunctionParametersAndBody(tail, Seq(), (parameters, body, bodyTail) => {
          val function = FunctionExpr(name, parameters, body)
          success(function, env.+(name -> function), bodyTail)
        }, failure)


      /* Assignments */
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: SymbolToken("=") :~: tail =>
        verifyType(typeName, env).right.flatMap(parentType =>
          parse(tail, env, (e, _, stream) => parentType.isChild(e.t) match {
            case true => success(DefExpr(name, e), env + (name -> e), stream)
            case false => failure(s"'$name' has the expected type $parentType, but was assigned to type ${e.t}")
          }, failure)
        )

      case SymbolToken(name) :~: SymbolToken("=") :~: tail =>
        parse(tail, env, (e, _, stream) => success(DefExpr(name, e), env + (name -> e), stream), failure)

    }
  }

  private def parseLoop(tokens : LiveStream[Token], env : ParserEnv, success: SuccessCont, failure: String => Value) : Value = {
    def parseValueToken(value : Token) : Either[String, Expr] = {
      value match {
        case SymbolToken(name) => env.get(name) match {
          case Some(NumberExpr(x)) => Right(NumberExpr(x))
          case None => Left(Error.REFERENCE_NOT_FOUND(name))
        }
        case IntToken(value: Int) => Right(NumberExpr(value))
        case DoubleToken(value : Double) => Right(NumberExpr(value))
        case e => Left(Error.SYNTAX_ERROR("a numeric value or reference to a numeric value", e.toString))
      }
    }
    def parseLoopWithRange(counterName : String, fromToken : Token, toToken : Token, bodyTokens : LiveStream[Token], success: SuccessCont, failure: String => Value) : Value = {
      parseValueToken(fromToken).right.flatMap(from => {
        parseValueToken(toToken).right.flatMap(to => {
          parse(bodyTokens, env + (counterName -> from), (bodyExpr, _, bodyTail) => {
            success(LoopExpr(DefExpr(counterName, from), to, bodyExpr), env, bodyTail)
          }, failure)
        })
      })
    }

    tokens match {
      case fromToken :~: SymbolToken("to") :~: toToken :~: SymbolToken("using") :~: SymbolToken(counter) :~: tail =>
        parseLoopWithRange(counter, fromToken, toToken, tail, success, failure)

      case fromToken :~: SymbolToken("to") :~: toToken :~: tail =>
        parseLoopWithRange(DEFAULT_LOOP_COUNTER, fromToken, toToken, tail, success, failure)

      case toToken :~: SymbolToken("using") :~: SymbolToken(counter) :~: tail =>
        parseLoopWithRange(counter, IntToken(1), toToken, tail, success, failure)

      case toToken :~: tail =>
        parseLoopWithRange(DEFAULT_LOOP_COUNTER, IntToken(1), toToken, tail, success, failure)

      case tail => failure("Failed to parse loop. Expected to-token, got " + tail)
    }
  }

  private def parseParameters(tokens: LiveStream[Token], env : ParserEnv, success : SuccessCont, failure : FailureCont) : Value = {
    tokens match {
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: tail =>
        verifyType(typeName, env) match {
          case Right(t) =>
            val reference = RefExpr(name, t)
            success(reference, env.+(name -> reference), tail)
          case Left(error) => Left(error)
        }
      case SymbolToken(name) :~: tail => failure(Error.EXPECTED_TYPE_PARAMETERS(name))
    }
  }

  private def parseUntil(tokens: LiveStream[Token], token : Token, env : ParserEnv,
                         success : SuccessCont, failure: FailureCont): Value = {
    parseUntil(parse, tokens, stream => stream.head.toString.equals(token.toString), env, success, failure)
  }

  private def parseUntil(parseFunction : (LiveStream[Token], ParserEnv, SuccessCont, FailureCont) => Value,
                         tokens: LiveStream[Token], condition : LiveStream[Token] => Boolean, env : ParserEnv,
                         success : SuccessCont, failure : FailureCont, spillEnvironment : Boolean = false): Value = {
    var envVar : ParserEnv = env
    var seq = Seq[Expr]()
    var seqFail : Option[String] = None
    var seqTail : LiveStream[Token] = tokens
    def seqSuccess: SuccessCont = (e, v, s) => {
      seqTail = s
      Right((e, v))
    }
    def seqFailure: (String) => Value = (s) => {
      seqFail = Some(s)
      Left(s)
    }
    while (seqFail.isEmpty && !seqTail.isPlugged && !condition(seqTail)) {
      parseFunction(seqTail, envVar, seqSuccess, seqFailure) match {
         case Left(s) => seqFail = Some(s)
         case Right((e, newEnv)) =>
           if (e != UnitExpr) seq = seq :+ e
           envVar = newEnv
       }
    }
    if (!seqTail.isPlugged && condition(seqTail) ) {
      seqTail = seqTail.tail
    }
    seqFail.map(seqFailure).getOrElse(
      if (spillEnvironment) {
        success(BlockExpr(seq), envVar, seqTail)
      } else {
        success(BlockExpr(seq), env, seqTail)
      }
    )
  }

  private def verifyType(typeName : String, env : ParserEnv) : Either[String, AnyType] = {
    env.getType(typeName) match {
      case Some(typeObject) => Right(typeObject)
      case _ => Left(Error.TYPE_NOT_FOUND(typeName))
    }
  }

  private def verifySimilarTypes(functionName : String, expected : Seq[RefExpr], actual : Seq[Expr], env : ParserEnv) : Option[String] = {
    if (actual.size != expected.size) {
      Some(Error.EXPECTED_PARAMETER_NUMBER(functionName, expected.size, actual.size))
    } else {
      expected.zip(actual).collect {
        case (expectedParam, actualParam) if !expectedParam.t.isChild(actualParam.t) =>
          return Some(Error.TYPE_MISMATCH(expectedParam.t.toString, actualParam.t.toString, s"calling '$functionName'"))
      }
      None
    }
  }

}