package ru.spbau.jvm.scala.multiset

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

class MultiSetTest extends FlatSpec with Matchers {
  def toList[K](set: MultiSet[K]): ListBuffer[K] = {
    val result = new ListBuffer[K]
    for (e <- set) result += e
    result
  }

  def copy[K](set: MultiSet[K]): MultiSet[K] = set.filter(_ => true)

  val emptySet = new MultiSet[Int]()
  val set = new MultiSet[Int](4, 23, 8, 42, 15, 42, 16)

  "An empty set" should "have size 0" in {
    emptySet.size() should be (0)
  }

  it should "be converted to empty list" in {
    toList(emptySet) should be (new ListBuffer[Int]())
  }

  "Constructor" should "work correctly" in {
    set.size() should be (7)
    toList(set) should be (ListBuffer[Int](4, 8, 15, 16, 23, 42, 42))
  }

  "Map" should "work correctly" in {
    toList(set.map(x => String.valueOf(x))) should be (ListBuffer[String]("15", "16", "23", "4", "42", "42", "8"))
  }

  "Clear" should "work correctly" in {
    val newSet = set.map(x => x)
    newSet.clear()
    newSet.size() should be (0)
    toList(newSet) should be (ListBuffer[Int]())
  }

  "toString" should "work correctly" in {
    set.toString should be ("[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2]")
  }

  "contains" should "work correctly" in {
    set.contains(0) should be (false)
    set.contains(15) should be (true)
    set.contains(42) should be (true)
  }

  "count and apply" should "work correctly" in {
    set.count(0) should be (0)
    set(0) should be (0)
    set.count(15) should be (1)
    set(15) should be (1)
    set.count(42) should be (2)
    set(42) should be (2)
  }

  "remove and removeAll" should "work correctly" in {
    val newSet = copy(set)
    val newSet2 = copy(set)
    newSet.remove(0)
    toList(newSet) should be (ListBuffer[Int](4, 8, 15, 16, 23, 42, 42))
    newSet.remove(16)
    toList(newSet) should be (ListBuffer[Int](4, 8, 15, 23, 42, 42))
    newSet.remove(42)
    toList(newSet) should be (ListBuffer[Int](4, 8, 15, 23, 42))
    newSet2.removeAll(42)
    toList(newSet2) should be (ListBuffer[Int](4, 8, 15, 16, 23))
  }

  "&" should "work correctly" in {
    val set1 = new MultiSet[Int](42)
    (set & set1).toString should be ("[42 -> 3]")
  }

  "|" should "work correctly" in {
    val set1 = new MultiSet[Int](108)
    (set | set1).toString should be ("[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2, 108 -> 1]")
  }

  "iterator" should "work correctly" in {
    val it = set.iterator()
    it.hasNext should be (true)
    it.next should be (4)
    it.hasNext should be (true)
    it.next should be (8)
    it.hasNext should be (true)
    it.next should be (15)
    it.hasNext should be (true)
    it.next should be (16)
    it.hasNext should be (true)
    it.next should be (23)
    it.hasNext should be (true)
    it.next should be (42)
    it.hasNext should be (true)
    it.next should be (42)
    it.hasNext should be (false)
  }

  "flatMap" should "work correctly" in {
    def numbers(e: Int): MultiSet[Char] = {
      val s = String.valueOf(e)
      val r = new MultiSet[Char]()
      for (c <- s) r.add(c)
      r
    }
    def f(): MultiSet[Char] = {
      for {
        e <- set
        g <- numbers(e)
      } yield g
    }
    toList(f()) should be (ListBuffer[Char]('1', '1', '2', '2', '2', '3', '4', '4', '4', '5', '6', '8'))
  }
}