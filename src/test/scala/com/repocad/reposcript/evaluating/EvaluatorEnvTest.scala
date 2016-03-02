package com.repocad.reposcript.evaluating

import com.repocad.reposcript.parsing.{AnyType, NumberType, StringType, UnitType}
import org.scalatest.{FlatSpec, Matchers}

class EvaluatorEnvTest extends FlatSpec with Matchers {

  "An EvaluatorEnv" should "Add an expression under a name" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).map should equal(Map("test" -> Map(Signature(Nil, NumberType) -> 3)))
  }
  it should "Remove an expression with a name" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).-("test", Nil, NumberType).map should equal(Map())
  }
  it should "Allow overloaded definitions under the same name" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).add("test", Nil, UnitType, Unit).map should equal(Map("test" -> Map(
      Signature(Nil, NumberType) -> 3,
      Signature(Nil, UnitType) -> Unit
    )))
  }
  it should "Overwrite definitions with the same name and signature" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).add("test", Nil, NumberType, 6).map should equal(
      Map("test" -> Map(Signature(Nil, NumberType) -> 6))
    )
  }
  it should "Find an entry in the environment" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).get("test", Nil, NumberType) should equal(Some(3))
  }
  it should "Not find an entry with the wrong type argument" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 3).get("test", Nil, UnitType) should equal(None)
  }
  it should "Find the correct entry between overloaded entries under the same name" in {
    EvaluatorEnv.empty.add("test", Nil, UnitType, Unit)
      .add("test", Nil, NumberType, 5)
      .get("test", Nil, UnitType) should equal(Some(Unit))
  }
  it should "Find an entry with a subtype of an input parameter" in {
    EvaluatorEnv.empty.add("test", Seq(AnyType), StringType, "hi").get("test", Seq(NumberType), StringType) should equal(Some("hi"))
  }
  it should "Merge two environments with the same name, but different signatures" in {
    EvaluatorEnv.empty.add("test", Nil, NumberType, 2) ++ EvaluatorEnv.empty.add("test", Nil, UnitType, Unit) should equal(
      new EvaluatorEnv(Map("test" -> Map(Signature(Nil, NumberType) -> 2, Signature(Nil, UnitType) -> Unit)))
    )
  }

}
