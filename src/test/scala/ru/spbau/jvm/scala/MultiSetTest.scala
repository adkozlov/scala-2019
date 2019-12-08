package ru.spbau.jvm.scala

import org.scalatest.FunSuite

class MultiSetTest extends FunSuite {
  test("empty set") {
    val set = MultiSet[Int]()
    assert(set.isEmpty)
    assert(!set.notEmpty)
  }

  test("not empty set") {
    val set = MultiSet(1)
    assert(!set.isEmpty)
    assert(set.notEmpty)
  }

  test("create not empty set") {
    val set = MultiSet(1, 2, 3, 4)
    assert(set.contains(1))
    assert(set.contains(2))
    assert(set.contains(3))
    assert(set.contains(4))
  }

  test("create not empty set with repeating elements") {
    val set = MultiSet(1, 2, 3, 4, 2, 3, 4, 3, 4, 4)
    assert(set(1) == 1)
    assert(set(2) == 2)
    assert(set(3) == 3)
    assert(set(4) == 4)
  }

  test("remove elements from set") {
    val set = MultiSet(1, 2, 3)
    assert(contains(set - 1, 2, 3))
    assert(!(set - 1).contains(1))
    assert(contains(set - 1 - 2, 3))
    assert(!(set - 1 - 2).contains(2))
    assert(!(set - 1 - 2 - 3).contains(3))
  }

  test("remove repeating elements from set") {
    val set = MultiSet(1, 2, 2)
    assert(contains(set - 2, 1, 2))
    assert(contains(set - 2 - 2, 1))
    assert(!contains(set - 2 - 2, 2))
  }

  test("map int to string") {
    val set = MultiSet(1, 2, 3, 4, 5)
    val setResult = MultiSet("1", "2", "3", "4", "5")
    assert(set.map(_.toString) == setResult)
  }

  test("const map") {
    val set = MultiSet(1, 2, 3, 4, 5)
    assert(set.map(_ => 1) == MultiSet(1, 1, 1, 1, 1))
  }


  test("try really big set") {
    var set = MultiSet.empty[Int]()
    for (elem <- 0 until 100) {
      for (_ <- elem until 100) {
        set = set + elem
      }
    }

    for (elem <- 0 until 100) {
      assert(set.contains(elem))
      assert(set(elem) == (100 - elem))
    }

    for (elem <- 0 until 100) {
      for (_ <- elem until 100) {
        set = set - elem
      }
      assert(!set.contains(elem))
    }
  }

  test("unite sets") {
    val set1 = MultiSet(1, 2, 3, 4, 5, 6)
    val set2 = MultiSet(1, 2, 3, 4, 5, 7)
    val set3 = set1 | set2

    assert(contains(set3, 1, 2, 3, 4, 5, 6, 7))
    assert(set3(1) == 2)
    assert(set3(2) == 2)
    assert(set3(3) == 2)
    assert(set3(4) == 2)
    assert(set3(5) == 2)
    assert(set3(6) == 1)
    assert(set3(7) == 1)
  }

  test("intersect sets") {
    val set1 = MultiSet(1, 2, 3, 4, 5, 6)
    val set2 = MultiSet(1, 3, 5, 7)
    val set3 = set1 & set2
    assert(set3 == MultiSet(1, 1, 3, 3, 5, 5))
  }

  test("filter") {
    val set1 = MultiSet(1, 2, 3, 4, 5, 6)
    val set3 = set1.filter(x => x % 2 == 0)
    assert(set3 == MultiSet(2, 4, 6))
  }

  test("filter repeated") {
    val set1 = MultiSet(1, 1, 2, 2, 3, 3, 4, 4, 4, 5, 6)
    val set3 = set1.filter(x => x % 2 == 0)
    assert(set3 == MultiSet(2, 2, 4, 4, 4, 6))
  }

  test("flatMap") {
    val set1 = MultiSet(1, 2, 3)
    val set2 = set1.flatMap(x => MultiSet(x, x, x))
    assert(set2 == MultiSet(1, 1, 1, 2, 2, 2, 3, 3, 3))
  }

  test("for-comprehension") {
    val set = MultiSet(1, 2, 3, 4, 5)
    val list = List(1, 2, 3, 4, 5)
    var setResult = MultiSet[Int]()
    for {
      a <- list
      b <- list
      c <- list
      x = a + b + c
      if x % 2 == 0
    } yield {
      setResult = setResult + x
    }
    val set2 = for {
      a <- set
      b <- set
      c <- set
      x = a + b + c
      if x % 2 == 0
    } yield x
    assert(set2 == setResult)
  }


  private def contains[T](set: MultiSet[T], elems: T*): Boolean = {
    var result = true
    for (elem <- elems) {
      result &= set.contains(elem)
    }
    result
  }
}
