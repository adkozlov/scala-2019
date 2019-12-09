package ru.spbau.jvm.scala.multiset

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {

  "MultiSet constructor" should "add all elems" in {
    val multiSet = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet.toString should be("[1 -> 3, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1]")
  }

  "MultiSet &" should "return intersection of multisets" in {
    val multiSet1 = MultiSet(1, 1, 1, 2, 3, 4, 5)
    val multiSet2 = MultiSet(1, 1, 3, 4, 5)
    (multiSet1 & multiSet2).toString should be("[1 -> 5, 3 -> 2, 4 -> 2, 5 -> 2]")
  }

  "MultiSet &" should "return sum of multisets" in {
    val multiSet1 = MultiSet(1, 1, 1, 2, 3, 4, 5)
    val multiSet2 = MultiSet(1, 1, 3, 4, 5)
    (multiSet1 | multiSet2).toString should be("[1 -> 5, 2 -> 1, 3 -> 2, 4 -> 2, 5 -> 2]")
  }

  "MultiSet apply" should "return count of elems" in {
    val multiSet = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet(1) should be(3)
  }

  "MultiSet add" should "add elem" in {
    val multiSet = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet.add(1)
    multiSet.add(42)
    multiSet == MultiSet(1, 1, 1, 1, 2, 3, 4, 5, 42) should be(true)
  }

  "MultiSet remove" should "remove elem" in {
    val multiSet = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet.remove(1)
    multiSet == MultiSet(1, 1, 2, 3, 4, 5) should be(true)
  }

  "MultiSet add & remove" should "correctly change size" in {
    val multiSet = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet.remove(1).add(2).remove(1).remove(2)
    multiSet.size() should be(5)
    multiSet.remove(42)
    multiSet.size() should be (5)
  }

  "MultiSet equals" should "return true with same sets" in {
    val multiSet1 = MultiSet(1, 1, 1, 2, 3, 4, 5)
    val multiSet2 = MultiSet(1, 1, 1, 2, 3, 4, 5)
    multiSet1 == multiSet2 should be(true)
  }

  "MultiSet map" should "return mapped multiset" in {
    val multiSet1 = MultiSet(1, 1, 1, 2, 3, 4, 5)
    val resultSet = MultiSet(2, 2, 2, 4, 6, 8, 10)
    multiSet1.map(_ * 2) == resultSet should be(true)
  }

}
