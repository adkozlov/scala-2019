package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  "fillFromList" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.toString should be(
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2]"
    )
  }

  "countOf" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.countOf(8) should be(1)
    multiSet.countOf(42) should be(2)
    multiSet.countOf(7) should be(0)
  }

  "intersect(&)" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val ms42 = MultiSet(42)
    (multiSet & ms42).toString should be("[42 -> 1]")
    (ms42 & multiSet).toString should be("[42 -> 1]")

    val ms42x2 = MultiSet(1, 42)
    (multiSet & ms42x2).toString should be("[42 -> 1]")
    (ms42x2 & multiSet).toString should be("[42 -> 1]")
  }

  "union(|)" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val ms108 = MultiSet(108)
    (multiSet | ms108).toString should be(
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2, 108 -> 1]"
    )
    (ms108 | multiSet).toString should be(
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2, 108 -> 1]"
    )
  }

  "map" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.toString should be(
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2]"
    )
    multiSet.map(x => x - 1).toString should be(
      "[3 -> 1, 7 -> 1, 14 -> 1, 15 -> 1, 22 -> 1, 41 -> 2]"
    )
  }
}
