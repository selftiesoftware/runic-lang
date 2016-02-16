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

              case SymbolToken("{") :~: _ => Left(Error.SYNTAX_ERROR("=", "}")(parameterState.tokens.head.position))

              // Extend objects
              //case SymbolToken("as")

              case SymbolToken("extends") :~: SymbolToken(parent) :~: objectTail =>
                startState.env.getAsType(parent, _.isInstanceOf[ObjectType]) match {
                  case Right(parent : ObjectType) =>
                    val hasParentParameters = parent.params.forall(parameter => parameterState.parameters.contains(parameter))
                    if (!hasParentParameters) {
                      Left(Error.EXPECTED_OBJECT_EXTENDS_PARAMETER(name, parent.toString, parent.params.toString(),
                        parameterState.parameters.toString())(parameterState.position))
                    } else {
                      val objectExpr = ObjectType(name, parameterState.parameters, parent)
                      success(ExprState(objectExpr, startState.env + (name -> objectExpr), objectTail))
                    }

                  case Right(element) => Left(Error.OBJECT_NOT_FOUND(parent)(parameterState.position))
                  case Left(error) => Left(error.apply(parameterState.position))
                }

              case SymbolToken("extends") :~: parentTail =>
                Left(Error.SYNTAX_ERROR("parent object name", parentTail.toString)(parentTail.head.position))

              case objectTail =>
                val objectExpr: ObjectType =
                  ObjectType(name, parameterState.parameters/* ++ parameterState.recursiveParameters.map(
                    parameterName => RefExpr(parameterName, TypeRef(name))
                  )*/, AnyType)
                success(ExprState(objectExpr, startState.env + (name -> objectExpr), objectTail))
            }

          case Left(error) => failure(error)
        }

      /* Assignments */
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: SymbolToken("=") :~: tail =>
        verifyTypeExists(typeName, startState).right.flatMap(parentType =>
          parse(startState.copy(env = startState.env.+(name -> parentType),tokens = tail), assignmentState => {
            val assignmentExpr = assignmentState.expr
            parentType.isChild(assignmentExpr.t) match {
              case true => success(ExprState(DefExpr(name, assignmentExpr),
                assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
              case false => failure(Error.ASSIGNMENT_TYPE_MISMATCH(name, parentType, assignmentExpr)(assignmentState.position))
            }
          }, failure)
        )

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
      case SymbolToken(name) :~: tail => failure(Error.EXPECTED_TYPE_PARAMETERS(name)(state.position))
      case tail => failure(Error.SYNTAX_ERROR("function parameters", tail.toString)(state.position))
    }
  }

}
