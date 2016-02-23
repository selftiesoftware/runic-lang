package com.repocad.reposcript.parsing

/**
 * Created by OS on 2/18/2016.
 */
object ErrorTest extends ParsingTest {

  def main(args: Array[String]) {
    //  println(AMBIGUOUS_TYPES[AnyType] ("xyz", Map(NumberType -> NumberExpr(5)))(Position.empty))

    def code = {
      "def sX = 15\n#def sY = 15\n\nline(0 0 sX sY)"
    }
    //println(parseString(code))
    println(parseStringAll(code))
  }
}
