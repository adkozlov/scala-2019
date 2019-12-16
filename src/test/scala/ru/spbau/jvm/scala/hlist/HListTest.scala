package ru.spbau.jvm.scala.hlist

import org.scalatest.{FlatSpec, Matchers}

class HListTest extends FlatSpec with Matchers {
  "zip" should "work correctly for empty lists" in {
    val zipped = HNil zip HNil
    zipped shouldBe HNil
  }

  "zip" should "work correctly if left list is empty" in {
    val zipped = HNil zip (1 :: 2 :: 3 :: HNil)
    zipped shouldBe HNil
  }

  "zip" should "work correctly if right list is empty" in {
    val zipped = (1 :: 2 :: 3 :: HNil) zip HNil
    zipped shouldBe HNil
  }

  "zip" should "work correctly with lists of equal size" in {
    val zipped = (1 :: 2 :: 3 :: HNil) zip ("one" :: "two" :: "three" :: HNil)
    zipped shouldBe (1, "one") :: (2, "two") :: (3, "three") :: HNil
  }

  "zip" should "work correctly if left list is shorter" in {
    val zipped = (1 :: 2 :: HNil) zip ("one" :: "two" :: "three" :: HNil)
    zipped shouldBe (1, "one") :: (2, "two") :: HNil
  }

  "zip" should "work correctly if right list is shorter" in {
    val zipped = (1 :: 2 :: 3 :: HNil) zip ("one" :: "two" :: HNil)
    zipped shouldBe (1, "one") :: (2, "two") :: HNil
  }

  "splitAt" should "work correctly for HNil" in {
    val splitted = HNil splitAt Zero
    splitted shouldBe (HNil, HNil)
  }

  "splitAt" should "work correctly for zero index" in {
    val splitted = (1 :: 2 :: HNil) splitAt Zero
    splitted shouldBe (HNil, 1 :: 2 :: HNil)
  }

  "splitAt" should "work correctly for non-empty list" in {
    val splitted = (1 :: 2 :: 3 :: HNil) splitAt Succ(Succ(Zero))
    splitted shouldBe (1 :: 2 :: HNil, 3 :: HNil)
  }

  "splitAt" should "not compile for too big indices" in {
    "(1 :: 2 :: HNil) splitAt Succ(Succ(Succ(Zero)))" shouldNot compile
  }
}
