package ru.spbau.jvm.scala.lecture06

class ZippableTest extends org.scalatest.FunSuite {
  test("Zip two empty lists") {
    assert((HNil zip HNil) == HNil)
  }

  test("Zip right empty list") {
    assert(((1 :: 2 :: HNil) zip HNil) == HNil)
  }

  test("Zip left empty list") {
    assert((HNil zip (1 :: HNil)) == HNil)
  }

  test("Zip one element lists") {
    val a = 1 :: HNil
    val b = 2 :: HNil
    val c = (1, 2) :: HNil
    assert((a zip b) == c)
  }

  test("Zip several element lists") {
    val a = 1 :: 1 :: 1 :: HNil
    val b = 2 :: 2 :: 2 :: HNil
    val c = (1, 2) :: (1, 2) :: (1, 2) :: HNil
    assert((a zip b) == c)
  }

  test("Zip with short left list") {
    val a = 100 :: 200 :: HNil
    val b = 2 :: 2 :: 2 :: HNil
    val c = (100, 2) :: (200, 2) :: HNil
    assert((a zip b) == c)
  }

  test("Zip with right short list") {
    val a = 1 :: 1 :: 1 :: HNil
    val b = 2 :: HNil
    val c = (1, 2) :: HNil
    assert((a zip b) == c)
  }

}
