package com.repocad.reposcript.parsing

/**
 * Created by OS on 2/18/2016.
 */
object ErrorTest extends ParsingTest {

  def main(args: Array[String]) {
    //  println(AMBIGUOUS_TYPES[AnyType] ("xyz", Map(NumberType -> NumberExpr(5)))(Position.empty))

    def code = {
      "Line(0 0 300 300)"
      //"repeat 0 to 6 using  \n{line(0 0 i 12)\n}"
    }
    //println(parseString(code))
    println(parseStringAll(code))
  }
}
