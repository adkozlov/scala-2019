package ru.spbau.jvm.scala

import org.scalatest.FlatSpec

class MultiSetTest extends FlatSpec {
  import MultiSetTest._

  "An empty set" should "have size 0" in {
    assert(MultiSet[Int]().size == 0)
  }

  "Constructor" should "work correctly" in {
    val set = MultiSet(1, 9, 4, 9, 5, 3, 2, 1, 1)
    assert(set.toList == List(1 -> 3, 2 -> 1, 3 -> 1, 4 -> 1, 5 -> 1, 9 -> 2))
  }

  "Plus" should "work correctly" in {
    val set = MultiSet(1, 2)
    val secondSet = set + 2 + 3
    assert(set.toList == List(1 -> 1, 2 -> 1))
    assert(secondSet.toList == List(1 -> 1, 2 -> 2, 3 -> 1))
  }

  it should "work with empty set" in {
    val set = MultiSet[Int]()
    assert((set + 2).toList == List(2 -> 1))
  }

  "Minus" should "work correctly" in {
    val set = MultiSet(2, 5, 2)
    val secondSet = set - 2
    val thirdSet = set - 5
    val fourthSet = set - 2 - 2 - 2

    assert(set.toList == List(2 -> 2, 5 -> 1))
    assert(secondSet.toList == List(2 -> 1, 5 -> 1))
    assert(thirdSet.toList == List(2 -> 2))
    assert(fourthSet.toList == List(5 -> 1))
  }

  it should "work with empty set" in {
    val set = MultiSet[Int]()
    assert((set - 1 - 2).toList == List())
  }

  "FoldLeft" should "work correctly" in {
    val array = Array(1, 2, 6, 7, 7, 1, 1)
    val set = MultiSet(array: _*)
    val foldlResult: List[Int] = set.foldLeft(List[Int]()) { (list, element) =>
      element :: list
    }.reverse
    assert(array.sorted.toList == foldlResult)
  }

  "Contains" should "work correctly" in {
    val set = MultiSet(-1, 0, -1, 44, 0, 100)
    assert(set.contains(-1))
    assert(set.contains(0))
    assert(!(set - 100).contains(100))
  }

  "Apply" should "work correctly" in {
    val set = MultiSet(4, 8, 15, 16, 23, 42, 42)
    assert(set(42) == 2)
    assert(set.count(42) == 2)
  }

  "Size" should "be right" in {
    val set = MultiSet(6, 3, 2, 3)
    assert(set.size == 4)
    assert((set + 4 + 3).size == 6)
    assert((set - 1).size == 4)
    assert((set - 3 - 3 - 3).size == 2)
  }

  "Set" should "be immutable" in {
    val set = MultiSet(1, 5, 5, 2)
    val newSet = set - 1 - 2 - 5 - 5
    assert(set.toList == List(1 -> 1, 2 -> 1, 5 -> 2))
    assert(newSet.toList == List())
  }

  "Union" should "work correctly" in {
    val firstSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val secondSet = MultiSet(4, 3, 42)
    assert((firstSet | secondSet).toList == List(3 -> 1, 4 -> 2, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 3))
  }

  it should "work with empty sets" in {
    val set = MultiSet(6, 3, 6)
    val emptySet = MultiSet[Int]()
    assert((set | emptySet).toList == set.toList)
    assert((emptySet | set).toList == set.toList)
    assert((emptySet | emptySet).toList == emptySet.toList)
  }

  "Intersect" should "work correctly" in {
    val firstSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    val secondSet = MultiSet(4, 3, 42, 16, 16)
    assert((firstSet & secondSet).toList == List(4 -> 1, 16 -> 1, 42 -> 1))
  }

  it should "work with empty sets" in {
    val set = MultiSet(6, 3, 6)
    val emptySet = MultiSet[Int]()
    assert((set & emptySet).toList == emptySet.toList)
    assert((emptySet & set).toList == emptySet.toList)
    assert((emptySet & emptySet).toList == emptySet.toList)
  }

  "Map" should "work correctly and reorder elements" in {
    val set: MultiSet[Short] = MultiSet(2, 3, 7, -1, 0, 7)
    val mappedSet: MultiSet[Int] = set.map { element =>
      element * -2
    }
    assert(mappedSet.toList == List(-14 -> 2, -6 -> 1, -4 -> 1, 0 -> 1, 2 -> 1))
  }

  "Flat map" should "work correctly and reorder elements" in {
    val set = MultiSet(2, 3, 7, 0, 7, 7, 7)
    var cnt = 7
    val newSet = set.flatMap { element =>
      cnt -= 1
      MultiSet(cnt + 1, cnt)
    }
    assert(newSet.toList == List(0 -> 1, 1 -> 2, 2 -> 2, 3 -> 2, 4 -> 2, 5 -> 2, 6 -> 2, 7 -> 1))
  }

  "Foreach" should "work correctly" in {
    val set = MultiSet(1, 2, 8, 2, 1, 1)
    var list: List[Int] = List()
    set.foreach { element =>
      list = element :: list
    }
    list = list.reverse
    assert(list == List(1, 1, 1, 2, 2, 8))
  }

  "Filter" should "inspect each element multiple times" in {
    val set = MultiSet(1, 1, 2, 3, 3, 1, 5)
    var cnt = 0
    val filteredSet = set.filter { _ =>
      cnt += 1
      if (cnt % 2 == 1) true else false
    }
    assert(filteredSet.toList == List(1 -> 2, 3 -> 1, 5 -> 1))
  }

  "Set" should "support for comprehension" in {
    val set = MultiSet(1, 1, 2, 3, 3, 1, 5)
    val newSet = for {
      element <- set
      if element != 5
      if element != 1
      listElement <- MultiSet(element, element + 1)
    } yield listElement
    assert(newSet.toList == List(2 -> 1, 3 -> 3, 4 -> 2))
  }
}

object MultiSetTest {
  implicit class MultiSetExt[A](val x: MultiSet[A]) extends AnyVal {
    def toList: List[(A, Int)] = x.foldLeftPairs(List[(A, Int)]()) { (list, value) =>
      value :: list
    }.reverse
  }
}


