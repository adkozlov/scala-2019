package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class SplittableTest extends FlatSpec with Matchers {
  "Split by zero" should "return HNil and list" in {
    val list = true :: "abra" :: 42.0 :: 322 :: HNil
    list.splitAt(Zero) should be(HNil, list)
  }

  "Split by valid value" should "split List by two parts" in {
    val list = true :: "abra" :: 42.0 :: 322 :: HNil
    list.splitAt(Suc(Zero)) should be(true :: HNil, "abra" :: 42.0 :: 322 :: HNil)
  }

  "Split by last value" should "return list and HNil" in {
    val list = true :: "abra" :: HNil
    list.splitAt(Suc(Suc(Zero))) should be(list, HNil)
  }

  "Split by invalid value" should "not compile" in {
    val list = true :: "abra" :: HNil
    //    list.splitAt(Suc(Suc(Suc(Zero))))
  }

}
