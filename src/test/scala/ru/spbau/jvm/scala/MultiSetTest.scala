package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  "fillFromList" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet.toString should be(
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2]"
    )
  }

  "equal" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    multiSet should be(MultiSet(4, 8, 15, 16, 23, 42, 42))
    multiSet should be(MultiSet(42, 4, 15, 8, 23, 42, 16))
  }

  "remove" should "ok" in {
    var multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42, 42)

    multiSet = multiSet.remove(42)
    multiSet should be(MultiSet(4, 8, 15, 16, 23, 42, 42))

    multiSet = multiSet.remove(42)
    multiSet should be(MultiSet(4, 8, 15, 16, 23, 42))
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
    (multiSet & ms42) should be(MultiSet(42))
    (ms42 & multiSet) should be(MultiSet(42))

    val ms42x2 = MultiSet(42, 42)
    (multiSet & ms42x2) should be(MultiSet(42, 42))
    (ms42x2 & multiSet) should be(MultiSet(42, 42))
  }

  "union(|)" should "ok" in {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val ms108 = MultiSet(108)
    (multiSet | ms108) should be(MultiSet(4, 8, 15, 16, 23, 42, 42, 108))

    (ms108 | multiSet) should be(MultiSet(4, 8, 15, 16, 23, 42, 42, 108))
  }

  "map" should "ok" in {
    val multiSet = MultiSet(1, 4, 1, 3, 3, 3)
    multiSet should be(MultiSet(1, 1, 3, 3, 3, 4))

    multiSet.map(x => x + 1) should be(MultiSet(2, 2, 4, 4, 4, 5))

    multiSet.map(x => if (x == 1) 5 else x) should be(
      MultiSet(3, 3, 3, 4, 5, 5)
    )
  }

  "filter" should "ok" in {
    MultiSet(1, 4, 1, 3, 3, 3).filter(x => x % 2 == 0) should be(MultiSet(4))

    MultiSet(4, 8, 15, 16, 23, 42, 42).filter(x => x % 2 == 0) should be(
      MultiSet(4, 8, 16, 42, 42)
    )
  }
}
