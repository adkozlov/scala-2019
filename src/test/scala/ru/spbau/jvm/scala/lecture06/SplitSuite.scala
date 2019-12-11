package ru.spbau.jvm.scala.lecture06

import org.scalatest.FunSuite

class SplitSuite extends FunSuite {

  test("Zero index split.") {
    val list = 1 :: 2 :: 3 :: "four" :: HNil
    val index = Zero
    val (res1, res2) = list.splitAt(index)
    assert(res1 == 1 :: HNil)
    assert(res2 == 2 :: 3 :: "four" :: HNil)
  }

  test("Nonzero index split.") {
    val list = 1 :: 2 :: 3 :: 4 :: 5 :: HNil

    val (l1, r1) = list.splitAt(PlusOne(Zero))
    assert(l1 == 1 :: 2 :: HNil)
    assert(r1 == 3 :: 4 :: 5 :: HNil)

    val (l2, r2) = list.splitAt(PlusOne(PlusOne(Zero)))
    assert(l2 == 1 :: 2 :: 3 :: HNil)
    assert(r2 == 4 :: 5 :: HNil)

    val (l3, r3) = list.splitAt(PlusOne(PlusOne(PlusOne(Zero))))
    assert(l3 == 1 :: 2 :: 3 :: 4 :: HNil)
    assert(r3 == 5 :: HNil)

    val (l4, r4) = list.splitAt(PlusOne(PlusOne(PlusOne(PlusOne(Zero)))))
    assert(l4 == 1 :: 2 :: 3 :: 4 :: 5 :: HNil)
    assert(r4 == HNil)

    //list.splitAt(PlusOne(PlusOne(PlusOne(PlusOne(PlusOne(Zero))))))
  }
}
