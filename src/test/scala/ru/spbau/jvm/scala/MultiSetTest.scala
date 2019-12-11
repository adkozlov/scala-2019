package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  "fillFromList" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.toString should be("[ 4->1  8->1  15->1  16->1  23->1  42->2 ]")
  }

  "countOf" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.countOf(8) should be(1)
    multiSet.countOf(42) should be(2)
    multiSet.countOf(7) should be(0)
  }

  "map" should "ok" in {
    val multiSet = MultiSet(1, 4, 1, 3, 3, 3)
    multiSet.toString should be("[ 1->2  3->3  4->1 ]")

    multiSet.map(x => x + 1).toString should be("[ 2->2  4->3  5->1 ]")

    multiSet.map(x => if (x == 1) 5 else x).toString should be(
      "[ 3->3  5->2  4->1 ]"
    )
  }

  "filter" should "ok" in {
    MultiSet(1, 4, 1, 3, 3, 3).filter(x => x % 2 == 0).toString should be(
      "[ 4->1 ]"
    )

    MultiSet(4, 8, 15, 16, 23, 42,
      42).filter(x => x % 2 == 0).toString should be(
      "[ 4->1  8->1  16->1  42->2 ]"
    )
  }

  "intersect(&)" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val ms42 = MultiSet(42)
    (multiSet & ms42).toString should be("[ 42->1 ]")
    (ms42 & multiSet).toString should be("[ 42->1 ]")

    val ms42x2 = MultiSet(42, 42)
    (multiSet & ms42x2).toString should be("[ 42->2 ]")
    (ms42x2 & multiSet).toString should be("[ 42->2 ]")
  }

  "union(|)" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val ms108 = MultiSet(108)
    (multiSet | ms108).toString should be(
      "[ 4->1  8->1  15->1  16->1  23->1  42->2  108->1 ]"
    )
    (ms108 | multiSet).toString should be(
      "[ 4->1  8->1  15->1  16->1  23->1  42->2  108->1 ]"
    )
  }
}
