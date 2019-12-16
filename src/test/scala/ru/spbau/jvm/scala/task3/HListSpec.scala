package ru.spbau.jvm.scala.task3

import org.scalatest.{FlatSpec, Matchers}

class HListSpec extends FlatSpec with Matchers {
  private val left = 0 :: 1 :: 2 :: 3 :: HNil
  private val right = "a" :: "b" :: "c" :: "d" :: HNil
  private val short = "foo" :: "bar" :: HNil

  "Split" should "split correctly" in {
    left.splitAt(Suc(Suc(Zero))) should be(0 :: 1 :: HNil, 2 :: 3 :: HNil)
  }

  "Split at zero" should "return Nil and the same list" in {
    left.splitAt(Zero) should be(HNil, left)
    HNil.splitAt(Zero) should be(HNil, HNil)
  }

  "Split at last" should "return the same list and Nil" in {
    left.splitAt(Suc(Suc(Suc(Suc(Zero))))) should be(left, HNil)
    HNil.splitAt(Zero) should be(HNil, HNil)
  }

  "Zip" should "work" in {
    left.zip(right) should be((0, "a") :: (1, "b") :: (2, "c") :: (3, "d") :: HNil)
  }

  "Zip" should "skip remaining elements" in {
    left.zip(short) should be((0, "foo") :: (1, "bar") :: HNil)
    short.zip(left) should be(("foo", 0) :: ("bar", 1) :: HNil)
  }

  "Zip" should "work with empty lists" in {
    left.zip(HNil) should be(HNil)
    HNil.zip(right) should be(HNil)
  }
}
