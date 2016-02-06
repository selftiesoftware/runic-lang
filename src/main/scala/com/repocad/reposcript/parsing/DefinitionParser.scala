package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing._

/**
  * Parses definitions of values, functions and objects.
  */
trait DefinitionParser extends TypedParser with ParserInterface with BlockParser {

  def parseDefinition(outerState : ParserState, success : SuccessCont, failure : FailureCont) : Value = {
    def parseFunctionParametersAndBody(innerState : ParserState, success : (Seq[RefExpr], Expr,
      LiveStream[Token]) => Value, failure : FailureCont) : Value = {
      parseFunctionParameters(innerState.tokens, outerState, (parameters, parameterTokens) => {
        parameterTokens match {
          case SymbolToken("=") :~: functionTail =>
            parseFunctionBody(innerState.copy(env = innerState.env, tokens = functionTail), parameters,
              bodyState => success(parameters, bodyState.expr, bodyState.tokens), failure)
          case tail => failure(Error.SYNTAX_ERROR("=", tail.toString)(innerState.position))
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
      /* Prepended function */
      case PunctToken("(") :~: tail => parseFunctionParameters(tail, outerState, (firstParams, firstTail) => {
        firstTail match {
          case SymbolToken(name) :~: PunctToken("(") :~: functionTail =>
            parseFunctionParametersAndBody(outerState.copy(tokens = functionTail), (secondParams, body, bodyTail) => {
              val parameters = firstParams ++ secondParams
              val function = FunctionType(name, parameters, body)
              success(ParserState(function, outerState.env.+(name -> function), bodyTail))
            }, failure)

          case SymbolToken(name) :~: SymbolToken("=") :~: functionTail =>
            parseFunctionBody(outerState.copy(tokens = functionTail), firstParams, bodyState => {
              val function = FunctionType(name, firstParams, bodyState.expr)
              success(ParserState(function, outerState.env.+(name -> function), bodyState.tokens))
            }, failure)
        }
      }, failure)

      /* Function and object definition */
      case SymbolToken(name) :~: PunctToken("(") :~: tail =>
        parseFunctionParameters(tail, outerState, (parameters, paramsTail) => {
          paramsTail match {
            case SymbolToken("=") :~: bodyTokens =>
              parseFunctionBody(outerState.copy(tokens = bodyTokens), parameters, bodyState => {
                val function = FunctionType(name, parameters, bodyState.expr)
                success(ParserState(function, outerState.env.+(name -> function), bodyState.tokens))
              }, failure)

            case SymbolToken("{") :~: _ => failure(Error.SYNTAX_ERROR("=", "}")(paramsTail.head.position))

            // Extend objects
            //case SymbolToken("as")

            case objectTail =>
              val objectExpr = ObjectType(name, parameters, AnyType)
              success(ParserState(objectExpr, outerState.env + (name -> objectExpr), objectTail))
          }
        }, failure)


      /* Assignments */
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: SymbolToken("=") :~: tail =>
        verifyTypeExists(typeName, outerState).right.flatMap(parentType =>
          parse(outerState.copy(tokens = tail), assignmentState => {
            val assignmentExpr = assignmentState.expr
            parentType.isChild(assignmentExpr.t) match {
              case true => success(ParserState(DefExpr(name, assignmentExpr),
                assignmentState.env + (name -> assignmentExpr), assignmentState.tokens))
              case false => failure(Error.ASSIGNMENT_TYPE_MISMATCH(name, parentType, assignmentExpr)(assignmentState.position))
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

  def parseFunctionParameters(parameterTokens : LiveStream[Token], outerState : ParserState,
                              success : (Seq[RefExpr], LiveStream[Token]) => Value, failure : FailureCont) : Value = {
    parseUntil(parseParameters, _.head.tag.toString == ")", ParserState(UnitExpr, outerState.env, parameterTokens),
      parametersState => { parametersState.expr match {
        case BlockExpr(exprs) if exprs.contains((e : Expr) => !e.isInstanceOf[RefExpr]) =>
          failure(Error.EXPECTED_PARAMETERS(parameterTokens.toString)(parametersState.position))
        case BlockExpr(exprs) =>
          success(exprs.asInstanceOf[Seq[RefExpr]], parametersState.tokens)
        case unknown => failure(Error.EXPECTED_PARAMETERS(unknown.toString)(parametersState.position))
      }
      }, failure)
  }

  private def parseParameters(state : ParserState, success : SuccessCont, failure : FailureCont) : Value = {
    state.tokens match {
      case SymbolToken(name) :~: SymbolToken("as") :~: SymbolToken(typeName) :~: tail =>
        state.env.getAsType(typeName, AnyType) match {
          case Right(typeExpr : AnyType) =>
            val reference = RefExpr(name, typeExpr)
            success(ParserState(reference, state.env.+(name -> reference), tail))
          case Right(expr) =>
            val reference = RefExpr(name, expr.t)
            success(ParserState(reference, state.env.+(name -> reference), tail))
          case Left(error) => failure(error.apply(state.position))
        }
      case SymbolToken(name) :~: tail => failure(Error.EXPECTED_TYPE_PARAMETERS(name)(state.position))
    }
  }

}
