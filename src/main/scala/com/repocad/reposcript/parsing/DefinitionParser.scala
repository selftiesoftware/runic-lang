package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._

/**
  * Parses definitions of values, functions and objects.
  */
trait DefinitionParser extends TypedParser with ParserInterface with BlockParser {

  def parseDefinition(startState: ExprState, success: SuccessCont[ExprState],
                      failure: FailureCont[ExprState]): Value[ExprState] = {
    startState.tokens match {
      /* Functions or objects */
      /* Prepended function */
      case PunctToken("(") :~: tail =>
        val parametersEither = parseFunctionParameters(DefinitionState("", startState.env, tail),
          parameterState => {
            parameterState.tokens match {
              case SymbolToken(name) :~: PunctToken("(") :~: parameterTail =>
                parseFunctionParameters(parameterState.copy(name = name, tokens = parameterTail), secondParameterState => {
                  Right(secondParameterState)
                }, error => Left(failure(error).left.get))
              case SymbolToken(name) :~: SymbolToken("=") :~: functionTail =>
                Right(parameterState.copy(name = name, tokens = functionTail))
            }
          }, error => Left(error))

        parametersEither match {
          case Right(parameters) =>
            parse(ExprState(UnitExpr, parameters.env, parameters.tokens), bodyState => {
              val function = FunctionType(parameters.name, parameters.parameters, bodyState.expr)
              success(ExprState(function, startState.env + (function.name -> function), bodyState.tokens))
            }, failure)
          case Left(error) => failure(error)
        }

      /* Function and object definition */
      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        parseFunctionParameters(DefinitionState(name, startState.env, tail), parameterState => Right(parameterState),
          error => Left(error)) match {

          case Right(parameterState) =>
            parameterState.tokens match {
              case SymbolToken("=") :~: bodyTokens =>
                parse(ExprState(UnitExpr, parameterState.env, bodyTokens), bodyState => {
                  val function = FunctionType(name, parameterState.parameters, bodyState.expr)
                  success(ExprState(function, startState.env.+(name -> function), bodyState.tokens))
                }, failure)

              case SymbolToken("{") :~: _ => Left(ParserError.SYNTAX_ERROR("=", "}")(parameterState.tokens.head.position))

              case SymbolToken("extends") :~: SymbolToken(parent) :~: PunctToken("(") :~: parentTail =>
                parseUntilToken[ExprState](ExprState(UnitExpr, startState.env, parentTail), ")", accumulateExprState, parse,
                  parentState => {
                    parentState.expr match {
                      case BlockExpr(parentParmeters) =>
                        parseObject(name, parameterState.copy(env = startState.env, tokens = parentState.tokens), Some(parent -> parentParmeters), success, failure)
                      case UnitExpr =>
                        parseObject(name, parameterState.copy(env = startState.env, tokens = parentState.tokens), Some(parent -> Seq()), success, failure)
                      case expr: Expr =>
                        parseObject(name, parameterState.copy(env = startState.env, tokens = parentState.tokens), Some(parent -> Seq(expr)), success, failure)
                    }
                  }, failure)
              //                  parentState => Right(parentState), error => Left(error)) match {
              //                  case Right(parentState) => parseObject(name, parameterState, Some(parentState), success, failure)
              //                  case Left(error) => failure(error)
              //                }
              case SymbolToken("extends") :~: SymbolToken(parent) :~: objectTail =>
                parseObject(name, parameterState.copy(tokens = objectTail), Some(parent -> Seq()), success, failure)

              case SymbolToken("extends") :~: parentTail =>
                Left(ParserError.SYNTAX_ERROR("parent object name", parentTail.toString)(parentTail.head.position))

              case objectTail => parseObject(name, parameterState.copy(env = startState.env), None, success, failure)
            }

          case Left(error) => failure(error)
        }

      /* Assignments */
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: SymbolToken("=") :~: tail =>
        verifyTypeExists(typeName, startState).right.flatMap(parentType => {
          parse(startState.copy(env = startState.env.+(name -> parentType), tokens = tail), assignmentState => {
            val assignmentExpr = assignmentState.expr
            parentType.isChild(assignmentExpr.t) match {
              case true => success(ExprState(DefExpr(name, assignmentExpr),
                assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
              case false => failure(ParserError.ASSIGNMENT_TYPE_MISMATCH(name, parentType, assignmentExpr)(assignmentState.position))
            }
          }, failure)
        }).left.flatMap(failure)

      case SymbolToken(name) :~: SymbolToken("=") :~: tail =>
        parse(startState.copy(tokens = tail), assignmentState => {
          val assignmentExpr = assignmentState.expr
          success(ExprState(DefExpr(name, assignmentExpr),
            assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
        }, failure)
      //success(DefExpr(name, e), env + (name -> e), stream), failure)
    }
  }

  private def parseFunctionParameters(startState: DefinitionState, success: SuccessCont[DefinitionState],
                                      failure: FailureCont[DefinitionState]): Value[DefinitionState] = {
    parseUntilToken[DefinitionState](startState, ")", accumulateDefinitions, parseParameters, success, failure)
  }

  private def accumulateDefinitions(first: DefinitionState, second: DefinitionState): DefinitionState = {
    second
  }

  private def parseParameters(state: DefinitionState, success: SuccessCont[DefinitionState],
                              failure: FailureCont[DefinitionState]): Value[DefinitionState] = {
    state.tokens match {
      // Recursive parameter referencing the same name as the function / object being defined
      //      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: tail if typeName == state.name =>
      //        success(DefinitionState(state.name, state.parameters, state.recursiveParameters :+ name,
      //          state.env.+(name -> AnyType), tail))
      // Regular non-recursive parameters...
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: tail =>
        state.env.getAsType(typeName, AnyType) match {
          case Right(typeExpr: AnyType) =>
            val reference = RefExpr(name, typeExpr)
            success(DefinitionState(state.name, state.parameters :+ reference, state.recursiveParameters,
              state.env.+(name -> reference), tail))
          case Right(expr) =>
            val reference = RefExpr(name, expr.t)
            success(DefinitionState(state.name, state.parameters :+ reference, state.recursiveParameters,
              state.env.+(name -> reference), tail))
          case Left(error) => failure(error.apply(state.position))
        }
      case SymbolToken(name) :~: tail => failure(ParserError.EXPECTED_TYPE_PARAMETERS(name)(state.position))
      case tail => failure(ParserError.SYNTAX_ERROR("function parameters", tail.toString)(state.position))
    }
  }

  private def parseObject(name: String, parameterState: DefinitionState, parentOption: Option[(String, Seq[Expr])],
                          success: SuccessCont[ExprState], failure: FailureCont[ExprState]): Value[ExprState] = {
    parentOption match {
      case Some((parentName, parentParameters)) =>
        parameterState.env.getAsType(parentName, _.isInstanceOf[ObjectType]) match {
          case Right(parent: ObjectType) =>
            createObjectFromParameters(name, parent, parameterState.parameters, parentParameters, parameterState.position) match {
              case Right(obj) => success(ExprState(obj, parameterState.env + (name -> obj), parameterState.tokens))
              case Left(error) => failure(error)
            }
          //              .right.flatMap(obj => success(ExprState(obj, parameterState.env + (name -> obj), parameterState.tokens)))
          //success(ExprState(UnitExpr, parameterState.env, parameterState.tokens))

          case Right(element) => failure(ParserError.OBJECT_NOT_FOUND(parentName)(parameterState.position))
          case Left(error) => failure(error.apply(parameterState.position))
        }

      case None =>
        val objectExpr = ObjectType(name, parameterState.parameters, AnyType)
        success(ExprState(objectExpr, parameterState.env + (name -> objectExpr), parameterState.tokens))
    }
  }

  private def createObjectFromParameters(objectName: String, parent: ObjectType,
                                         parameters: Seq[RefExpr], defaultParameters: Seq[Expr],
                                         position: Position): Either[ParserError, ObjectType] = {
    val expectedParameters = parent.params.size
    val actualParameters = parameters.size + defaultParameters.size
    if (expectedParameters != actualParameters) {
      Left(ParserError.EXPECTED_PARAMETER_NUMBER(parent.name, expectedParameters.toString,
        actualParameters.toString)(position))
    } else {
      val parametersWithoutValue = parent.params.filter(t => !parameters.contains(t))

      if (parametersWithoutValue.size != defaultParameters.size) {
        Left(ParserError.EXPECTED_PARAMETER_NUMBER(objectName, parametersWithoutValue.size + " default",
          defaultParameters.toString())(position))
      } else {

        // Verify default parameters (and implicitly the object parameters)
        var verifiedDefaultParameters: Either[ParserError, Map[String, Expr]] = Right(Map())
        var i = 0
        while (i < parametersWithoutValue.size && verifiedDefaultParameters.isRight) {
          val parameterWithoutValue = parametersWithoutValue(i)
          val defaultParameter = defaultParameters(i)
          if (parameterWithoutValue.t.isChild(defaultParameter.t)) {
            verifiedDefaultParameters =
              Right(verifiedDefaultParameters.right.get + (parameterWithoutValue.name -> defaultParameter))
          } else {
            verifiedDefaultParameters =
              Left(ParserError.TYPE_MISMATCH(parameterWithoutValue.t.toString, defaultParameter.t.toString,
                "setting default parameters for object " + objectName)(position))
          }
          i = i + 1
        }

        verifiedDefaultParameters.right.map(verifiedDefaultParameters => {
          ObjectType(objectName, parameters, parent, verifiedDefaultParameters)
        })
      }
    }
  }

}
