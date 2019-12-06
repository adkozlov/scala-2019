package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class MultiSetSpec extends FlatSpec with Matchers {

  "MultiSet size" should "equals to element number" in {
    val multiSet = new MultiSet(1, 2, 2, 3)

    multiSet.size() should be(4)

    multiSet.remove(2)
    multiSet.remove(1)
    multiSet.remove(3)

    multiSet.size() should be(1)
  }

  "MultiSet" should "support intersection" in {
    val multiSet1 = new MultiSet(1, 2, 3, 3)
    val multiSet2 = new MultiSet(2, 3, 4)

    (multiSet1 & multiSet2).toString should be("[2 -> 1, 3 -> 1]")
  }

  "MultiSet" should "support union" in {
    val multiSet1 = new MultiSet(1, 2, 3, 3)
    val multiSet2 = new MultiSet(2, 3, 3, 3, 4)

    (multiSet1 | multiSet2).toString should be("[1 -> 1, 2 -> 1, 3 -> 3, 4 -> 1]")
  }

  "MultiSet" should "support for-comprehension" in {
    val multiSet = new MultiSet(3, 2, 2, 1)

    val res = for (v <- multiSet if v > 1) yield v

    res.toString should be ("[2 -> 2, 3 -> 1]")
  }

}
