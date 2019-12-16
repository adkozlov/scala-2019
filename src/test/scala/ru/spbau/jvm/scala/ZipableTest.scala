package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class ZipableTest extends FlatSpec with Matchers {
  "Zip of lists with same length" should "return zipped list" in {
    val first = true :: "abra" :: 42.0 :: 322 :: HNil
    val second = false :: "kadabra" :: 322 :: 42.0 :: HNil
    first.zip(second) should be((true, false) :: ("abra", "cadabra") :: (42.0, 322) :: (322, 42.0) :: HNil)
  }

  "Zip of list with HNil" should "return HNil" in {
    val first = true :: "abra" :: 42.0 :: 322 :: HNil
    first.zip(HNil) should be(HNil)
  }

  "Zip of HNil with list" should "return HNil" in {
    val second = true :: "abra" :: 42.0 :: 322 :: HNil
    HNil.zip(second) should be(HNil)
  }

  "Zip of lists with different length" should "work correct" in {
    val first = false :: HNil
    val second = true :: "abra" :: 42.0 :: 322 :: HNil
    first.zip(second) should be((false, true) :: HNil)
    second.zip(first) should be((true, false) :: HNil)
  }
}
