package ru.spbau.jvm.scala.multiset

import org.scalatest.FlatSpec

class TestMultiSet extends FlatSpec {
  "Element count " should " be correct in Multiset" in {
    val set = MultiSet(4, 8, 15, 16, 23, 42, 42)
    assert(set(42) === 2)
  }

  "If element not exists, it " should " be added as new Node in MultiSet" in {
    val set = MultiSet(1, 2, 3)
    assert(set(4) === 0)
    set.add(4)
    assert(set(4) === 1)
  }

  "If element existed in set, new item " should " be added to the same node in MultiSet" in {
    val set = MultiSet(1, 2, 3)
    assert(set(3) === 1)
    set.add(3)
    assert(set(3) === 2)
  }

  "If all element instances are removed, they " should " not exist in MultiSet" in {
    val set = MultiSet(1, 2, 3, 4, 5, 6, 7)
    assert(set(3) === 1)
    set.remove(3)
    assert(set(3) === 0)
    assert(!set.contains(3))
  }

  "Remove " should " not delete all elements" in {
    val set = MultiSet(1, 2, 3, 4, 5, 5, 5)
    assert(set(5) === 3)
    set.remove(5, 2)
    assert(set(5) === 1)
    assert(set.contains(5))
  }

  "Union " should " keep max count for each element" in {
    val set1 = MultiSet(1, 2, 2, 3, 3, 3, 4)
    val set2 = MultiSet(3, 2, 2, 1, 1, 1)
    val set3 = set1 | set2
    assert(set3(1) === 3)
    assert(set3(2) === 2)
    assert(set3(3) === 3)
    assert(set3(4) === 1)
  }

  "Intersection " should " keep min count for each element" in {
    val set1 = MultiSet(1, 2, 2, 3, 3, 3, 4)
    val set2 = MultiSet(3, 2, 2, 1, 1, 1)
    val set3 = set1 & set2
    assert(set3(1) === 1)
    assert(set3(2) === 2)
    assert(set3(3) === 1)
    assert(set3(4) === 0)
  }

  "MultiSet " should " be empty " in {
    val set = MultiSet[Int]()
    assert(set.isEmpty)
  }

  "Iterator " should " return value for each instance " in {
    val set = MultiSet(1, 2, 2, 3)
    assert(set.iterator.size === 4)
  }

  "Map " should "apply function for each element" in {
    val set = MultiSet(1, 2, 2, 3)
    val quads = set.map(x => x * x)
    assert(quads(1) === 1)
    assert(quads(4) === 2)
    assert(quads(9) === 1)
  }

  "ToString " should "contain all elements and counts" in {
    val set = MultiSet(1, 2, 2, 3)
    val str = set.toString
    assert(str.contains("1 -> 1"))
    assert(str.contains("2 -> 2"))
    assert(str.contains("3 -> 1"))
  }

}
