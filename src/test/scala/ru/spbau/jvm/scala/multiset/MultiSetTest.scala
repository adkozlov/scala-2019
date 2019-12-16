package ru.spbau.jvm.scala.multiset

import org.scalatest.FlatSpec

class MultiSetTest extends FlatSpec{

  import MultiSetTest._

  "toString" should "work correctly" in {
    assert(emptySet.toString == "[]")
    assert(smallSet.toString == "[a -> 2, b -> 1, c -> 1]")
    assert(mediumSet.toString == "[0 -> 1, 1 -> 4, 2 -> 2, 3 -> 1, 5 -> 1]")
  }

  "toList" should "work correctly" in {
    assert(emptySet.toList == List())
    assert(smallSet.toList == List(("a", 2), ("b", 1), ("c", 1)))
    assert(mediumSet.toList == List((0, 1), (1, 4), (2, 2), (3, 1), (5, 1)))
  }

  "count and apply" should "wort correctly for non-existing values" in {
    assert(emptySet.count(30) == 0)
    assert(smallSet.count("30") == 0)
    assert(smallSet.count("d") == 0)
    assert(mediumSet.count(-1) == 0)
    assert(mediumSet.count(4) == 0)
    assert(mediumSet.count(7) == 0)
    assert(emptySet(30) == 0)
    assert(smallSet("30") == 0)
    assert(smallSet("d") == 0)
    assert(mediumSet(-1) == 0)
    assert(mediumSet(4) == 0)
    assert(mediumSet(7) == 0)
  }

  "+" should "wort correctly" in {
    var set = MultiSet[String]()
    assert(set.toList == List())
    set = set + "d"
    assert(set.toList == List(("d", 1)))
    set = set + "q"
    assert(set.toList == List(("d", 1), ("q", 1)))
    set = set + "b"
    assert(set.toList == List(("b", 1), ("d", 1), ("q", 1)))
    set = set + "f"
    assert(set.toList == List(("b", 1), ("d", 1), ("f", 1), ("q", 1)))
    set = set + "d"
    assert(set.toList == List(("b", 1), ("d", 2), ("f", 1), ("q", 1)))
    set = set + "q"
    assert(set.toList == List(("b", 1), ("d", 2), ("f", 1), ("q", 2)))
  }

  "add" should "wort correctly" in {
    var set = MultiSet[String]()
    assert(set.toList == List())
    set = set.add("d", 3)
    assert(set.toList == List(("d", 3)))
    set = set.add("q", 5)
    assert(set.toList == List(("d", 3), ("q", 5)))
    set = set.add("b", 2)
    assert(set.toList == List(("b", 2), ("d", 3), ("q", 5)))
    set = set.add("f")
    assert(set.toList == List(("b", 2), ("d", 3), ("f", 1), ("q", 5)))
    set = set.add("d", 12)
    assert(set.toList == List(("b", 2), ("d", 15), ("f", 1), ("q", 5)))
    set = set.add("q", 100)
    assert(set.toList == List(("b", 2), ("d", 15), ("f", 1), ("q", 105)))
  }

  "delete and deleteAll" should "wort correctly" in {
    var set = mediumSet
    assert(set.toList == List((0, 1), (1, 4), (2, 2), (3, 1), (5, 1)))
    set = set.delete(1, 2)
    assert(set.toList == List((0, 1), (1, 2), (2, 2), (3, 1), (5, 1)))
    set = set.delete(1, 5)
    assert(set.toList == List((0, 1), (2, 2), (3, 1), (5, 1)))
    set = set.deleteAll(2)
    assert(set.toList == List((0, 1), (3, 1), (5, 1)))
  }

  "-" should "wort correctly" in {
    var set = smallSet
    assert(set.toList == List(("a", 2), ("b", 1), ("c", 1)))
    set = set - "d"
    assert(set.toList == List(("a", 2), ("b", 1), ("c", 1)))
    set = set - "b"
    assert(set.toList == List(("a", 2), ("c", 1)))
    set = set - "b"
    assert(set.toList == List(("a", 2), ("c", 1)))
    set = set - "a"
    assert(set.toList == List(("a", 1), ("c", 1)))
    set = set - "a"
    assert(set.toList == List(("c", 1)))
    set = set - "c"
    assert(set.toList == List())
  }

  "foreach" should "keep order of elements" in {
    var testList = List[Int]()
    mediumSet.foreach(x => { testList = x :: testList })
    assert(testList == List(5, 3, 2, 1, 0))
  }

  "map" should "not break tree if it changes order" in {
    val set = mediumSet.map(-_ * 3)
    assert(set.toList == List((-15, 1), (-9, 1), (-6, 2), (-3, 4), (0, 1)))
  }

  "map" can "change type" in {
    val set = mediumSet.map(x => (-x * 3).toString)
    assert(set.toList == List(("-15", 1), ("-3", 4), ("-6", 2), ("-9", 1), ("0", 1)))
  }

  "filter" should "work correctly" in {
    assert(mediumSet.filter(_ % 2 == 1).toList == List((1, 4), (3, 1), (5, 1)))
  }

  "&" should "count values correctly" in {
    assert((emptySet & emptySet).toList == emptySet.toList)
    assert((smallSet & smallSet).toList == smallSet.toList)
    assert((mediumSet & mediumSet).toList == mediumSet.toList)
    assert((emptySet & mediumSet).toList == emptySet.toList)

    assert((MultiSet(1, 1, 1, 2, 4, 4, 5, 5, 7) & MultiSet(9, 6, 6, 6, 3, 0, 0)).toList == List())
    assert((MultiSet(1, 1, 1, 2, 4, 4, 5, 5, 7) & MultiSet(9, 7, 7, 7, 4, 1, 1)).toList
      == List((1, 2), (4, 1), (7, 1)))
  }

  "|" should "count values correctly" in {
    assert((emptySet | emptySet).toList == emptySet.toList)
    assert((smallSet | smallSet).toList == List(("a", 4), ("b", 2), ("c", 2)))
    assert((mediumSet | mediumSet).toList == List((0, 2), (1, 8), (2, 4), (3, 2), (5, 2)))
    assert((emptySet | mediumSet).toList == mediumSet.toList)

    assert((MultiSet(1, 1, 1, 2, 4, 4, 5, 5, 7) | MultiSet(9, 6, 6, 6, 3, 0, 0)).toList
      == List((0, 2), (1, 3), (2, 1), (3, 1), (4, 2), (5, 2), (6, 3), (7, 1), (9, 1)))
    assert((MultiSet(1, 1, 1, 2, 4, 4, 5, 5, 7) | MultiSet(9, 7, 7, 7, 4, 1, 1)).toList
      == List((1, 5), (2, 1), (4, 3), (5, 2), (7, 4), (9, 1)))
  }
}

object MultiSetTest {
  private val emptySet = MultiSet[Int]()
  private val smallSet = MultiSet("a", "b", "c", "a")
  private val mediumSet = MultiSet(1, 2, 0, 1, 1, 5, 3, 2, 1)
}