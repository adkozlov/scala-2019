package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {

  "MultiSet" should "support add and remove elements" in {
    val set = MultiSet[Int]()

    set.add(1) should be(true)
    set.add(2) should be(true)
    set.size() should be(2)

    set.add(1) should be(true)
    set.add(2) should be(true)
    set.size() should be(4)

    set.contains(1) should be(true)
    set.contains(2) should be(true)
    set.contains(3) should be(false)

    set.remove(1) should be(true)
    set.size() should be(3)
    set.remove(1) should be(true)
    set.remove(1) should be(false)
    set.size() should be(2)
  }

  "MultiSet" should "support union" in {
    val multiSet1 = MultiSet(1, 2, 3, 3, 4)
    val multiSet2 = MultiSet(2, 3, 3, 3, 4)

    val unionTree = multiSet1 | multiSet2
    unionTree.toString should be("[1 -> 1, 2 -> 1, 3 -> 3, 4 -> 1]")
    unionTree.size() should be(6)
  }

  "MultiSet" should "support intersection" in {
    val multiSet1 = MultiSet(1, 2, 3, 3)
    val multiSet2 = MultiSet(2, 3, 4, 5)

    val intersectTree = multiSet1 & multiSet2
    intersectTree.toString should be("[2 -> 1, 3 -> 1]")
    intersectTree.size() should be(2)
  }

}