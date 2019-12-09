package ru.spbau.jvm.scala.hw2

import org.scalatest.FunSuite

class MultisetTest extends FunSuite {
  test("An empty multiset content") {
    val multiset = new Multiset[Int]()
    assert(multiset.toString() == "[]")
  }

  test("Single element multiset content") {
    val multiset = new Multiset(1)
    assert(multiset.toString() == "[1 -> 1]")
  }

  test("Single element added twice multiset content") {
    val multiset = new Multiset(1, 1)
    assert(multiset.toString() == "[1 -> 2]")
  }

  test("Several string elements multiset") {
    val multiset = new Multiset("a", "b", "c")
    assert(multiset.toString == """[a -> 1, b -> 1, c -> 1]""")
  }

  test("Empty multiset and") {
    val a = new Multiset(1, 2, 3)
    val b = new Multiset(4, 5, 6)
    assert((a & b).toString == "[]")
  }

  test("One element in multiset cross") {
    val a = new Multiset(1, 2, 3)
    val b = new Multiset(1, 5, 6)
    assert((a & b).toString == "[1 -> 1]")
  }

  test("Two copies of one element in multiset cross") {
    val a = new Multiset(3, 3, 3)
    val b = new Multiset(3, 5, 3)
    assert((a & b).toString == "[3 -> 2]")
  }

  test("Simple multiset union") {
    val a = new Multiset(1)
    val b = new Multiset(4)
    assert((a | b).toString == "[1 -> 1, 4 -> 1]")
  }

  test("Multiset union count") {
    val a = new Multiset(1, 2, 1)
    val b = new Multiset(4, 1, 2)
    assert((a | b).toString == "[1 -> 3, 2 -> 2, 4 -> 1]")
  }

  test("Add element to empty multiset") {
    val a = new Multiset[Int]()
    a.add(1)
    assert(a.toString == "[1 -> 1]")
  }

  test("Add copy of an element") {
    val a = new Multiset(1, 2, 1, 2)
    a.add(1)
    assert(a.toString == "[1 -> 3, 2 -> 2]")
  }

  test("Add new element") {
    val a = new Multiset(1, 2)
    a.add(3)
    assert(a.toString == "[1 -> 1, 2 -> 1, 3 -> 1]")
  }

  test("Remove existing element from multiset") {
    val a = new Multiset(1)
    a.remove(1)
    assert(a.toString == "[]")
  }

  test("Remove nonexistent element from multiset") {
    val a = new Multiset(1)
    a.remove(2)
    assert(a.toString == "[1 -> 1]")
  }

  test("Remove one of several same elements from multiset") {
    val a = new Multiset(1, 1, 1)
    a.remove(1)
    assert(a.toString == "[1 -> 2]")
  }

  test("Multiset map test") {
    val a = new Multiset(1, 2)
    val b = a.map(x => x * 2)
    assert(b.toString == "[2 -> 1, 4 -> 1]")
  }

  test("Multiset map with type change") {
    val a = new Multiset(1, 2, 2)
    val b = a.map(x => "a" * x)
    assert(b.toString == "[a -> 1, aa -> 2]")
  }

  test("Multiset filter") {
    val a = new Multiset(1, 2, 3, 2, 3)
    val b = a.filter(x => x >= 2)
    assert(b.toString == "[2 -> 2, 3 -> 2]")
  }

  test("Empty filter result") {
    val a = new Multiset(1, 2, 3, 2, 3)
    val b = a.filter(x => x > 10)
    assert(b.toString == "[]")
  }

  test("Useless filter") {
    val a = new Multiset(1, 2, 3, 2, 3)
    val b = a.filter(x => x < 10)
    assert(b.toString == "[1 -> 1, 2 -> 2, 3 -> 2]")
  }

  test("For each") {
    val a = new Multiset(0, 1, 2, 3)
    val b = Array(0, 0, 0, 0)
    a.foreach(x => b(x) += 1)
    assert(b.toList == Array(1, 1, 1, 1).toList)
  }

  test("For each with same elements") {
    val a = new Multiset(0, 2, 2, 0)
    val b = Array(0, 0, 0, 0)
    a.foreach(x => b(x) += 1)
    assert(b.toList == Array(2, 0, 2, 0).toList)
  }

  test("Contains existing element in set of one element") {
    val a = new Multiset("aa")
    assert(a.contains("aa"))
  }

  test("Contains nonexistent element in set of one element") {
    val a = new Multiset("aa")
    assert(!a.contains("bb"))
  }

  test("Contains existing element after remove") {
    val a = new Multiset("aa", "bb", "a", "aa", "cd", "asd", "pj")
    assert(a.contains("aa"))
    a.remove("aa")
    assert(a.contains("aa"))
    a.remove("aa")
    assert(!a.contains("aa"))
  }

  test("Contains after add and remove") {
    val a = new Multiset(1, 2, 3, 4)
    assert(!a.contains(10))
    a.add(10)
    assert(a.contains(10))
  }
}
