package ru.spbau.jvm.scala.hlist

import org.scalatest.{FlatSpec, Matchers}

class HListSplitTests extends FlatSpec with Matchers {
  private def testList() = 1 :: 2 :: 3 :: true :: HNil

  "Split at start" should "return same list" in {
    val list = testList();
    list.splitAt(Zero) should be(HNil, list)
  }

  "Split at end" should "return same list" in {
    val list = testList();
    list.splitAt(Next(Next(Next(Next(Zero))))) should be(list, HNil)
  }

  "Split first" should "split list correctly" in {
    val list = testList();
    list.splitAt(Next(Zero)) should be(1 :: HNil, 2 :: 3 :: true :: HNil)
  }

  "Split at index more than end" should "not compile" in {
    val list = testList();
//    list.splitAt(Next(Next(Next(Next(Next(Next(Next(Zero))))))))
  }
}
