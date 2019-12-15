package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class ZipSpec extends FlatSpec with Matchers {
  private val defaultList = "5" :: 1 :: true :: 0.5 :: HNil
  private val otherList = 7 :: 2 :: 0.1 :: false :: HNil
  private val oneElementList = "3" :: HNil

  "HNil zip HNil" should "be HNil" in {
    HNil zip HNil should be(HNil)
  }

  "HNil zip list" should "be HNil" in {
    HNil zip defaultList should be(HNil)
    HNil zip oneElementList should be(HNil)
  }

  "list zip HNil" should "be HNil" in {
    defaultList zip HNil should be(HNil)
    oneElementList zip HNil should be(HNil)
  }

  "zipping lists with different size" should "work correctly" in {
    defaultList zip oneElementList should be(("5", "3") :: HNil)
    oneElementList zip defaultList should be(("3", "5") :: HNil)
  }

  "zipping list with same size" should "work correctly" in {
    defaultList zip otherList should be(("5", 7) :: (1, 2) :: (true, 0.1) :: (0.5, false) :: HNil)
    otherList zip defaultList should be((7, "5") :: (2, 1) :: (0.1, true) :: (false, 0.5) :: HNil)
  }

  "zipping list with itself" should "work correctly" in {
    defaultList zip defaultList should be(("5", "5") :: (1, 1) :: (true, true) :: (0.5, 0.5) :: HNil)
    otherList zip otherList should be((7, 7) :: (2, 2) :: (0.1, 0.1) :: (false, false) :: HNil)
    oneElementList zip oneElementList should be(("3", "3") :: HNil)
  }
}