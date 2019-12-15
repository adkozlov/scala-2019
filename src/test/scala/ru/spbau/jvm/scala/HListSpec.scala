package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class HListSpec extends FlatSpec with Matchers {
  private val list = true :: 1 :: "true" :: 1.0 :: HNil
  private val left = 0 :: 1 :: 2 :: 3 :: HNil
  private val right = "zero" :: "one" :: "two" :: "three" :: HNil
  private val notFullRight = "zero" :: "one" :: HNil

  "Split at zero" should "return NIL and same list" in {
    list.splitAt(Zero) should be (HNil, list)
  }

  "Split at last" should "return same list and NIL" in {
    list.splitAt(Succ(Succ(Succ(Succ(Zero))))) should be (list, HNil)
  }

  "Split at half" should "split list at two halves" in {
    list.splitAt(Succ(Succ(Zero))) should be (true :: 1 :: HNil, "true" :: 1.0 :: HNil)
  }

  "Split at invalid index" should "not compile" in {
    // list.splitAt(Succ(Succ(Succ(Succ(Succ(Zero))))))
  }

  "Zip" should "work" in {
    left.zip(right) should be ((0, "zero") :: (1, "one") :: (2, "two") :: (3, "three") :: HNil)
  }

  "Zip" should "discard remaining elements" in {
    left.zip(notFullRight) should be ((0, "zero") :: (1, "one") :: HNil)
    notFullRight.zip(left) should be (("zero", 0) :: ("one", 1) :: HNil)
  }

  "Zip" should "work with empty lists" in {
    left.zip(HNil) should be (HNil)
    HNil.zip(right) should be (HNil)
    HNil.zip(HNil) should be (HNil)
  }
}
