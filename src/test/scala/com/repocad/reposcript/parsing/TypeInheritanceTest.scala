package com.repocad.reposcript.parsing

class TypeInheritanceTest extends ParsingTest {

  "A type" should "inherit from AnyType" in {
    AnyType.isChild(NumberType) should equal(true)
  }
  it should "be child of the same type" in {
    NumberType.isChild(NumberType) should equal(true)
  }
  it should "can not be a child of another type" in {
    NumberType.isChild(StringType) should equal(false)
  }
  it should "be a child of a higher type" in {
    case object DerivedType extends AnyType {
      val parent: AnyType = AnyType
      val t: AnyType = this
    }
    case object DerivedSubType extends AnyType {
      val parent = DerivedType
      val t = this
    }
    DerivedType.isChild(DerivedSubType) should equal(true)
  }


}
