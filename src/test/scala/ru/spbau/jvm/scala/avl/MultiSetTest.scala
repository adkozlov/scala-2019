package ru.spbau.jvm.scala.avl

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  private def testSet1() = MultiSet[Int](1 :: 2 :: 3 :: 4 :: 5 :: 5 :: 9 :: 9 :: Nil)

  private def testSet2() = MultiSet[Int](4 :: 5 :: 6 :: 6 :: 9 :: Nil)

  "Constructor" should "be correct" in {
    testSet1().toString should be("[1 -> 1, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 2]")
    MultiSet[Int](Nil).toString should be("[]")
  }

  "add" should "be correct" in {
    MultiSet.empty[Int].add(0).toString should be("[0 -> 1]")
    MultiSet.empty[Int].add(-1).add(0).add(1).add(12).add(1).toString should be("[-1 -> 1, 0 -> 1, 1 -> 2, 12 -> 1]")
  }

  "remove" should "be correct" in {
    MultiSet.empty[Int].add(0).remove(0).toString should be("[]")
    MultiSet.empty[Int].add(-1).add(0).add(1).add(12).add(1).remove(1).add(12).toString should be("[-1 -> 1, 0 -> 1, 1 -> 1, 12 -> 2]")
  }

  "size" should "be correct" in {
    MultiSet.empty[Int].add(0).remove(0).size() should be(0)
    MultiSet.empty[Int].add(-1).add(0).add(1).add(12).add(1).remove(1).add(12).size() should be(5)
  }

  "&" should "be correct" in {
    (testSet1() & testSet2()).toString should be("[4 -> 2, 5 -> 3, 9 -> 3]")
  }

  "|" should "be correct" in {
    (testSet1() | testSet2()).toString should be("[1 -> 1, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3, 6 -> 2, 9 -> 3]")
  }

  "map" should "be correct" in {
    testSet1().map(it => -it).toString should be("[-9 -> 2, -5 -> 2, -4 -> 1, -3 -> 1, -2 -> 1, -1 -> 1]")
  }
}
