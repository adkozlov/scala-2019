package ru.spbau.jvm.scala.lecture06

import org.scalatest.FunSuite


class HListTest extends FunSuite {

  test("Zip two simple lists.") {
    val list1 = 1 :: '2' :: "3" :: HNil
    val list2 = "4" :: 5 :: '6' :: HNil
    val zipped = list1 zip list2
    assert(zipped == (1, "4") :: ('2', 5) :: ("3", '6'):: HNil)
  }

  test("Zip two empty lists.") {
    val list1 = HNil
    val list2 = HNil
    val zipped = list1 zip list2
    assert(zipped == HNil)
  }

  test("Can not zip two lists with different size. Compilation error if uncomment.") {
    //Two lists with different size can not be zipped
    val list1 = 1 :: '2' :: "3" :: HNil
    val list2 = "4" :: 5 :: HNil
//    val zipped = list1 zip list2
//    val zipped = list2 zip list1
    val list3 = 1 :: '2' :: HNil
    val list4 = HNil
//    val zipped = list3 zip list4
//    val zipped = list4 zip list3
  }

  test("SplitAt middle index.") {
    val three = Succ(Succ(Succ(Zero)))
    val list = "hello" :: 1 :: 'a' :: 123 :: HNil
    val (left, right) = list.splitAt(three)
    assert(left == "hello" :: 1 :: 'a' :: HNil)
    assert(right == 123 :: HNil)
  }

  test("SplitAt zero index.") {
    val zero = Zero
    val list = "hello" :: 1 :: 'a' :: 123 :: HNil
    val (left, right) = list.splitAt(zero)
    assert(left == HNil)
    assert(right == list)
  }

  test("SplitAt last index.") {
    val four = Succ(Succ(Succ(Succ(Zero))))
    val list = "hello" :: 1 :: 'a' :: 123 :: HNil
    val (left, right) = list.splitAt(four)
    assert(left == list)
    assert(right == HNil)
  }

  test("SplitAt empty list.") {
    val zero = Zero
    val list = HNil
    val (left, right) = list.splitAt(zero)
    assert(left == HNil)
    assert(right == HNil)
  }

  test("can not splitAt index out of bounds. Compilation error if uncomment.") {
    val five = Succ(Succ(Succ(Succ(Succ(Zero)))))
    val list = "hello" :: 1 :: 'a' :: 123 :: HNil
//    val (left, right) = list.splitAt(five)
  }
}
