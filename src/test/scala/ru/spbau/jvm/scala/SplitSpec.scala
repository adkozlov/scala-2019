package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class SplitSpec extends FlatSpec with Matchers {
  private val defaultList = "5" :: 1 :: true :: 0.5 :: HNil
  private val oneElementList = "3" :: HNil
  private val one = Suc(Zero)
  private val two = Suc(one)
  private val three = Suc(two)
  private val four = Suc(three)

  "HNil splitAt Zero" should "be (HNil, HNil)" in {
    HNil splitAt Zero should be((HNil, HNil))
  }

  "list splitAt Zero" should "be (HNil, list)" in {
    defaultList splitAt Zero should be((HNil, defaultList))
    oneElementList splitAt Zero should be((HNil, oneElementList))
  }

  "list splitAt length" should "be (list, HNil)" in {
    defaultList splitAt four should be((defaultList, HNil))
    oneElementList splitAt one should be((oneElementList, HNil))
  }

  "split at the middle" should "work correctly" in {
    defaultList splitAt one should be(("5" :: HNil, 1 :: true :: 0.5 :: HNil))
    defaultList splitAt two should be(("5" :: 1 :: HNil, true :: 0.5 :: HNil))
    defaultList splitAt three should be(("5" :: 1 :: true :: HNil, 0.5 :: HNil))
  }

  "split at number more then length" should "not compile" in {
    assertDoesNotCompile("defaultList splitAt Suc(four)")
    assertDoesNotCompile("oneElementList splitAt two")
  }

}
