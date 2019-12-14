package ru.spbau.jvm.scala.heterogen

import org.scalatest.FunSuite

class ZipTest extends FunSuite {
  test("zip empty") {
    assert(HNil == HNil.zip(HNil))
  }

  test("zip left empty") {
    val right = 1 :: 2 :: 3 :: HNil
    assert(HNil == HNil.zip(right))
  }

  test("zip right empty") {
    val left = 1 :: 2 :: 3 :: HNil
    assert(HNil == left.zip(HNil))
  }

  test("zip non empty of same length") {
    val left = 1 :: true :: "false" :: HNil
    val right = "true" :: 0 :: false :: HNil
    val result = (1, "true") :: (true, 0) :: ("false", false) :: HNil

    assert(result == left.zip(right))
  }

  test("zip non empty of different length") {
    val left = 1 :: true :: "false" :: HNil
    val right = "true" :: 0 :: HNil
    val result = (1, "true") :: (true, 0) :: HNil

    assert(result == left.zip(right))
  }

  test("zip non empty of different length 2") {
    val right = 1 :: true :: "false" :: HNil
    val left = "true" :: 0 :: HNil
    val result = ("true", 1) :: (0, true) :: HNil

    assert(result == left.zip(right))
  }
}
