package com.repocad.reposcript.parsing

import com.repocad.reposcript.lexing.Position

/**
  * A class containing error messages
  */
case class Error(message : String, position : Position)

object Error {
  def AMBIGUOUS_TYPES(key: String, matches: Map[AnyType, Expr])(implicit position : Position) : Error =
    Error(s"${matches.size} matches found that matches types under the name $key. Please specify the type or remove the type restriction. Following ambiguous matches found: $matches", position)


  def ASSIGNMENT_TYPE_MISMATCH(name : String, parentType: AnyType, assignment : Expr)(implicit position : Position) : Error =
    Error(s"'$name' has the expected type $parentType, but was assigned to type ${assignment.t}", position)

  def EXPECTED_FUNCTION_PARAMETERS(name : String, expected : String, actual : String)(implicit position : Position) : Error =
    Error(s"Expected parameters for function $name like $expected, but got $actual", position)
  def EXPECTED_OBJECT_ACCESS(actual: String)(implicit position : Position) : Error =
    Error(s"Expected access to object, but tried to access the expression $actual", position)
  def EXPECTED_OBJECT_PARAMETERS(name : String, expected : String, actual : String)(implicit position : Position) : Error =
    Error(s"Expected call for the object $name like $expected, but got $actual", position)
  def EXPECTED_PARAMETERS(actual : String)(implicit position : Position) : Error =
    Error(s"Expected parameter list when creating a function or object, but received '$actual'", position)
  def EXPECTED_PARAMETER_NUMBER(functionName : String, expected : Int, actual : Int)(implicit position : Position) : Error =
    Error(s"Function '$functionName' requires $expected parameters, but $actual was given", position)
  def EXPECTED_TYPE_PARAMETERS(name : String)(implicit position : Position) : Error =
    Error(s"No type information for variable $name; please specify its type using '$name as [Type]'", position)

  def IMPORT_FAILED(scriptName : String, error : String)(implicit position : Position) : Error =
    Error(s"Script $scriptName failed to load with error: $error", position)

  def FUNCTION_NOT_FOUND(functionName: String)(implicit position : Position) : Error =
    Error(s"Function '$functionName' not found", position)

  def OBJECT_INSTANCE_NOT_FOUND(callName: String)(implicit position : Position) : Error =
    Error(s"Could not find object instance by the name of $callName", position)
  def OBJECT_MISSING_PARAMETERS(name : String)(implicit position : Position) : Error =
    Error(s"Object '$name' must have at least one parameter", position)
  def OBJECT_NOT_FOUND(name: String)(implicit position : Position) : Error =
    Error(s"Could not find object of name '$name'", position)
  def OBJECT_UNKNOWN_PARAMETER_NAME(objectName : String, accessor: String)(implicit position : Position) : Error =
    Error(s"No field in object $objectName by the name of $accessor", position)

  def REFERENCE_NOT_FOUND(reference : String)(implicit position : Position) : Error =
    Error(s"Could not find object '$reference'. Has it been defined?", position)

  def SYNTAX_ERROR(expected : String, actual : String)(implicit position : Position) : Error =
    Error(s"Syntax error: Expected '$expected', but found '$actual'", position)

  def TYPE_MISMATCH(expected : String, actual : String, when : String = "")(implicit position : Position) : Error =
    Error(s"Type mismatch ${if (when.isEmpty) "" else "when " + when}: Expected $expected, but got $actual", position)

  def TYPE_NOT_FOUND(typeName : String)(implicit position : Position) : Error =
    Error(s"Type '$typeName' not found in scope. Is it defined above?", position)

}
