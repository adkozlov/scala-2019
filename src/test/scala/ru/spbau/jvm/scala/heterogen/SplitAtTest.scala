package ru.spbau.jvm.scala.heterogen

import org.scalatest.FunSuite

class SplitAtTest extends FunSuite {
  test("splitAt Nil") {
    val list = HNil
    assert((HNil, HNil) == list.splitAt(Zero))
  }

  test("splitAt not empty list") {
    val list = 1 :: 2 :: 3 :: 4 :: HNil
    assert(list.splitAt(Zero) == (HNil, 1 :: 2 :: 3 :: 4 :: HNil))
    assert(list.splitAt(Suc(Zero)) == (1 :: HNil, 2 :: 3 :: 4 :: HNil))
    assert(list.splitAt(Suc(Suc(Zero))) == (1 :: 2 :: HNil, 3 :: 4 :: HNil))
    assert(list.splitAt(Suc(Suc(Suc(Zero)))) == (1 :: 2 :: 3 :: HNil, 4 :: HNil))
    assert(list.splitAt(Suc(Suc(Suc(Suc(Zero))))) == (1 :: 2 :: 3 :: 4 :: HNil, HNil))

    // Doesn't compile because length list < 5.
    // list.splitAt(Suc(Suc(Suc(Suc(Suc(Zero))))))
  }
}
