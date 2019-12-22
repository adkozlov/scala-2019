package ru.spbau.jvm.scala

import java.util

import org.scalatest.{FlatSpec, Matchers}

class MultiSetTest extends FlatSpec with Matchers {
  "Empty multiset" should "have zero size" in {
    assert(new MultiSet[BigInt]().size == 0)
  }

  "Non-empty multiset" should "have correct size" in {
    assert(new MultiSet[BigInt](1, 2, 3).size() == 3)
  }

  "Contains" should "return false on empty multiset" in {
    assert(!new MultiSet[BigInt]().contains(2))
  }

  "Contains" should "return false if value does not present" in {
    assert(!new MultiSet[BigInt](1, 2, 3).contains(4))
  }

  "Contains" should "return true if value presents" in {
    assert(new MultiSet[BigInt](1, 2, 3).contains(3))
  }

  "Constructor" should "construct correct tree" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](-3, -3, -3, -3, 1, 1, 2, 9, 9, 9))
    val multiSet = new MultiSet[BigInt](1, -3, 2, 9, 1, -3, -3, 9, -3, 9)
    val actual = new util.ArrayList[BigInt]()
    multiSet.foreach(value => actual.add(value))
    expected shouldBe actual
  }

  "Adding values" should "work correctly" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](-3, -3, -3, -3, 1, 1, 2, 9, 9, 9))
    val multiSet = new MultiSet[BigInt](1, -3, 9, 1, -3, -3, 9)
    multiSet.add(2)
    multiSet.add(-3)
    multiSet.add(9)
    val actual = new util.ArrayList[BigInt]()
    multiSet.foreach(value => actual.add(value))
    expected shouldBe actual
  }

  "Removing values" should "work correctly" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](-3, -3, -3, -3, 1, 1, 2, 9, 9, 9))
    val multiSet = new MultiSet[BigInt](1, -3, 9, 1, -3, -3, 9, 2, -3, 9, 0, 1, 2)
    multiSet.remove(2)
    multiSet.remove(0)
    multiSet.remove(1)
    val actual = new util.ArrayList[BigInt]()
    multiSet.foreach(value => actual.add(value))
    expected shouldBe actual
  }

  "Map" should "work correctly" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](1, 1, 4, 9, 9, 9, 9, 81, 81, 81))
    val multiSet = new MultiSet[BigInt](1, -3, 9, 1, -3, -3, 9, 2, -3, 9)
    val actual = new util.ArrayList[BigInt]()
    val mappedMultiSet = multiSet.map(value => value * value)
    mappedMultiSet.foreach(value => actual.add(value))
    expected shouldBe actual
  }

  "Intersect" should "work correctly" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](-3, -3, 1))
    val multiSet = new MultiSet[BigInt](1, -3, 9, 1, -3, -3, 9, 2, -3, 9)
    val multiSet2 = new MultiSet[BigInt](1, 0, -3, -3)
    val result = multiSet & multiSet2
    val actual = new util.ArrayList[BigInt]()
    result.foreach(value => actual.add(value))
    expected shouldBe actual
  }

  "Union" should "work correctly" in {
    val expected = new util.ArrayList[BigInt](util.Arrays.asList[BigInt](-3, -3, -3, -3, -3, -3, 0, 1, 1, 1, 2, 9, 9, 9))
    val multiSet = new MultiSet[BigInt](1, -3, 9, 1, -3, -3, 9, 2, -3, 9)
    val multiSet2 = new MultiSet[BigInt](1, 0, -3, -3)
    val result = multiSet | multiSet2
    val actual = new util.ArrayList[BigInt]()
    result.foreach(value => actual.add(value))
    expected shouldBe actual
  }
}
