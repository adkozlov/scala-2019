package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  private val initList = List(1, 2, 2, 3, 4, 5, 5, 9, 9, 9)

  "MultiSet" should "be correctly initialized" in {
    MultiSet[Int](initList : _*).toString should be ("[1 -> 1, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3]")
    MultiSet[Int](List.empty : _*).toString should be ("[]")
    MultiSet[Int](22).toString should be ("[22 -> 1]")
  }

  "add" should "work correctly" in {
    val testSet = MultiSet[Int]()

    testSet.add(1).toString should be ("[1 -> 1]")
    testSet.add(1).add(2).toString should be ("[1 -> 1, 2 -> 1]")
    testSet.add(1).add(2).add(2).toString should be ("[1 -> 1, 2 -> 2]")
    testSet.add(1).add(2).add(2).add(1).toString should be ("[1 -> 2, 2 -> 2]")
    testSet.add(1).add(2).add(2).add(1).add(-1).toString should be ("[-1 -> 1, 1 -> 2, 2 -> 2]")
  }

  "size" should "work correctly" in {
    val testSet = MultiSet[Int]()

    testSet.add(1).size should be (1)
    testSet.add(1).add(2).size should be (2)
    testSet.add(1).add(2).add(2).size should be (3)
    testSet.add(1).add(2).add(2).add(1).size should be (4)
    testSet.add(1).add(2).add(2).add(1).add(-1).size should be (5)

    val testSet1 = MultiSet[Int](initList : _*)
    testSet1.remove(1).size should be (9)
    testSet1.remove(1).remove(2).size should be (8)
    testSet1.remove(1).remove(2).remove(2).size should be (7)
    testSet1.remove(1).remove(2).remove(2).remove(5)
      .size should be (6)
    testSet1.remove(1).remove(2).remove(2).remove(5)
      .remove(9).size should be (5)
    testSet1.remove(1).remove(2).remove(2).remove(5)
      .remove(9).remove(5).size should be (4)
  }

  "remove" should "work correctly" in {
    val testSet = MultiSet[Int](initList : _*)

    testSet.remove(1).toString should be ("[2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3]")
    testSet.remove(1).remove(2).toString should be ("[2 -> 1, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3]")
    testSet.remove(1).remove(2).remove(2).toString should be ("[3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3]")
    testSet.remove(1).remove(2).remove(2).remove(5).toString should be ("[3 -> 1, 4 -> 1, 5 -> 1, 9 -> 3]")
    testSet.remove(1).remove(2).remove(2).remove(5).remove(9).toString should be ("[3 -> 1, 4 -> 1, 5 -> 1, 9 -> 2]")
    testSet.remove(1).remove(2).remove(2).remove(5).remove(9).remove(5).toString should be ("[3 -> 1, 4 -> 1, 9 -> 2]")
  }

  "&" should "work correctly" in {
    val testSet1 = MultiSet[Int](initList : _*)
    val testSet2 = MultiSet[Int](initList : _*)
    val testSet3 = MultiSet[Int]().add(228).add(322).add(228).add(1).add(2)

    (testSet1 & testSet2).toString should be ("[1 -> 1, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3]")
    (testSet1 & testSet3).toString should be ("[1 -> 1, 2 -> 1]")
  }

  "|" should "work correctly" in {
    val testSet1 = MultiSet[Int](initList : _*)
    val testSet2 = MultiSet[Int](initList : _*)
    val testSet3 = MultiSet[Int]().add(228).add(322).add(228)

    (testSet1 | testSet2).toString should be ("[1 -> 2, 2 -> 4, 3 -> 2, 4 -> 2, 5 -> 4, 9 -> 6]")
    (testSet1 | testSet3 | testSet2).toString should be ("[1 -> 2, 2 -> 4, 3 -> 2, 4 -> 2, 5 -> 4, 9 -> 6, 228 -> 2, 322 -> 1]")
    (testSet1 | testSet3).toString should be ("[1 -> 1, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 9 -> 3, 228 -> 2, 322 -> 1]")
  }

  "for comprehensions" should "work correctly" in {
    val testSet = MultiSet[Int](initList : _*)

    var counter = 0
    for (i <- testSet if testSet.contains(i)) {
      i should be (initList(counter))
      counter += 1
    }
  }

  "get from set" should "work correctly" in {
    val testSet = MultiSet[Int](initList : _*)

    testSet(1) should be (1)
    testSet(2) should be (2)
    testSet(9) should be (3)
  }

  "map" should "work correctly" in {
    val testSet = MultiSet[Int](initList : _*)

    testSet.map(it => 5 + it).toString should be ("[6 -> 1, 7 -> 2, 8 -> 1, 9 -> 1, 10 -> 2, 14 -> 3]")
    testSet.map(_ => 2).toString should be ("[2 -> 10]")
  }

  "flatMap" should "work correctly" in {
    val testSet = MultiSet[Int](initList : _*)

    testSet.flatMap(it => MultiSet(5 + it) | MultiSet(it)).toString should be ("[1 -> 1, 2 -> 2, 3 -> 1, 4 -> 1, 5 -> 2, 6 -> 1, " +
      "7 -> 2, 8 -> 1, 9 -> 4, 10 -> 2, 14 -> 3]")
  }
}
