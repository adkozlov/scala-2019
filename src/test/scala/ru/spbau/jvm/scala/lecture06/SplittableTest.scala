package ru.spbau.jvm.scala.lecture06

class SplittableTest extends org.scalatest.FunSuite {
  test("Split at zero") {
    val a = 1 :: 2 :: 3 :: HNil
    val (left, right) = a splitAt Zero
    assert(left == HNil)
    assert(right == a)
  }

  test("Split at first") {
    val a = 1 :: 2 :: 3 :: HNil
    val one = Next(Zero)
    val (left, right) = a splitAt one
    val b = 1 :: HNil
    val c = 2 :: 3 :: HNil
    assert(left == b)
    assert(right == c)
  }

  test("Split at middle") {
    val a = 1 :: 2 :: 3 :: 4 :: HNil
    val two = Next(Next(Zero))
    val (left, right) = a splitAt two
    val b = 1 :: 2 :: HNil
    val c = 3 :: 4 :: HNil
    assert(left == b)
    assert(right == c)
  }

  test("Split with empty tail") {
    val a = 1 :: 2 :: 3 :: HNil
    val three = Next(Next(Next(Zero)))
    val (left, right) = a splitAt three
    assert(left == a)
    assert(right == HNil)
  }
}
