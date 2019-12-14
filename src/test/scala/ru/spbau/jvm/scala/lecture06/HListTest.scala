package ru.spbau.jvm.scala
package lecture06

import org.scalatest.{FlatSpec, Matchers}

class HListTest extends FlatSpec with Matchers {
  private val list1 = "hello" :: 42 :: false :: "world" :: HNil
  private val list2 = 3 :: true :: "scala" :: HNil

  "Zip" should "return empty list if one of lists is empty" in {
    (HNil zip HNil) should be (HNil)
    (list1 zip HNil) should be (HNil)
    (HNil zip list2) should be (HNil)
  }

  it should "correctly zip lists with same sizes" in {
    (list1 zip list1) should be (("hello", "hello") :: (42, 42) :: (false, false) :: ("world", "world") :: HNil)
    (list2 zip list2) should be ((3, 3) :: (true, true) :: ("scala", "scala") :: HNil)
  }

  it should "correctly zip lists with different sizes" in {
    (list1 zip list2) should be (("hello", 3) :: (42, true) :: (false, "scala") :: HNil)
    (list2 zip list1) should be ((3, "hello") :: (true, 42) :: ("scala", false) :: HNil)
  }

  "SplitAt" should "work correctly on index 0" in {
    HNil.splitAt(Zero) should be ((HNil, HNil))
    list1.splitAt(Zero) should be ((HNil, list1))
    list2.splitAt(Zero) should be ((HNil, list2))
  }

  it should "work correctly on index which equals list size" in {
    list1.splitAt(Next(Next(Next(Next(Zero))))) should be ((list1, HNil))
    list2.splitAt(Next(Next(Next(Zero)))) should be ((list2, HNil))
  }

  it should "work correctly on inner indexes" in {
    list1.splitAt(Next(Next(Zero))) should be (("hello" :: 42 :: HNil, false :: "world" :: HNil))
    list1.splitAt(Next(Zero)) should be (("hello" :: HNil, 42 :: false :: "world" :: HNil))
    list2.splitAt(Next(Next(Zero))) should be ((3 :: true :: HNil, "scala" :: HNil))
  }

  it should "not compile on indexes greater than list size" in {
    assertDoesNotCompile("list2.splitAt(Next(Next(Next(Next(Zero)))))")
  }
}