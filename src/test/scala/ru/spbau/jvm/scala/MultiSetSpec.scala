package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class MultiSetSpec extends FlatSpec with Matchers {

  "constructor" should "add all elements" in {
    new MultiSet(1, 1, 1, 2, 2, 3).toString should be ("[1 -> 3, 2 -> 2, 3 -> 1]")
  }

  "add" should "add element" in {
    val set = new MultiSet(1, 1, 2)
    set.add(1)
    set.add(3)
    set.add(3)
    set.toString should be ("[1 -> 3, 2 -> 1, 3 -> 2]")
  }

  "remove" should "remove element" in {
    val set = new MultiSet(1, 1, 2)
    set.remove(2)
    set.add(3)
    set.remove(3)
    set.remove(1)
    set.add(2)
    set.add(0)
    set.remove(1)
    set.toString should be ("[0 -> 1, 2 -> 1]")
  }

  "clear" should "remove all elements" in {
    val set = new MultiSet(1, 2, 2, 1)
    set.clear()
    set.toString should be ("[]")
    set.getSize should be (0)
  }

  "getSize" should "return correct size" in {
    val set = new MultiSet(1, 2, 2, 1)
    set.getSize should be (4)
    set.remove(2)
    set.getSize should be (3)
    set.add(3)
    set.getSize should be (4)
    set.remove(3)
    set.getSize should be (3)
  }

  "apply" should "return count" in {
    val set = new MultiSet(1, 1, 1, 2, 2, 3)
    set(1) should be (3)
    set(2) should be (2)
    set(3) should be (1)
  }

  "equals" should "be true on similar sets" in {
    val set1 = new MultiSet(1, 2, 2, 1, 3, 4)
    val set2 = new MultiSet(1, 2, 2, 1, 3, 4)
    set1 == set2 should be (true)
    set1.add(6)
    set1 == set2 should be (false)
  }

  "|" should "return multiset union" in {
    val set1 = new MultiSet(1, 2, 2, 1, 3, 4)
    val set2 = new MultiSet(2, 4, 5, 6)
    (set1 | set2).toString should be ("[1 -> 2, 2 -> 3, 3 -> 1, 4 -> 2, 5 -> 1, 6 -> 1]")
  }

  "|" should "return multiset intersection" in {
    val set1 = new MultiSet(1, 2, 2, 1, 3, 4)
    val set2 = new MultiSet(2, 4, 5, 6)
    (set1 & set2).toString should be ("[2 -> 3, 4 -> 2]")
  }

  "for-comprehension" should "be available" in {
    val set1 = new MultiSet(1, 2, 2, 1, 3, 4)
    val set2 = for (x <- set1 if x < 4)
      yield x
    set2.toString should be ("[1 -> 2, 2 -> 2, 3 -> 1]")
  }

  "higher order functions" should "be available" in {
    val set1 = new MultiSet(1, 2, 2, 1, 3, 4)
    val result = new mutable.ArrayDeque[Int]()
    set1.map(x => x * x).filter(x => x < 10).foreach(x => result.append(x))
    result should be (mutable.ArrayDeque(1, 1, 4, 4, 9))
  }

}
