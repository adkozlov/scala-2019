package ru.spbau.jvm.scala.multiset

import org.scalatest._

class MultiSetSpec extends FlatSpec with Matchers {
  "Multiset" should "create from elements" in {
    MultiSet[Int](1, 2, 3).toString() shouldBe "[1 -> 1, 2 -> 1, 3 -> 1]"
  }

  "Multiset" should "create from multiple elements" in {
    MultiSet[Int](1, 2, 1, 1, 2, 3).toString() shouldBe "[1 -> 3, 2 -> 2, 3 -> 1]"
  }

  "Multiset" should "do & operation" in {
    (MultiSet[Int](1, 2, 3, 6) & MultiSet[Int](1, 3, 5)).toString shouldBe "[1 -> 1, 3 -> 1]"
  }

  "Multiset" should "do | operation" in {
    (MultiSet[Int](1, 2, 3, 6, 1) | MultiSet[Int](1, 3, 5)).toString shouldBe "[1 -> 3, 2 -> 1, 3 -> 2, 5 -> 1, 6 -> 1]"
  }

  "Multiset" should "correctly answer \"contains\" query" in {
    MultiSet[Int](1, 2, 3).contains(1) shouldBe true
    MultiSet[Int](1, 2, 3).contains(4) shouldBe false
  }

  "Multiset" should "correctly answer \"count\" query" in {
    MultiSet[Int](1, 2, 3, 1, 1, 2).count(1) shouldBe 3
    MultiSet[Int](1, 2, 3, 1, 1, 2).count(2) shouldBe 2
    MultiSet[Int](1, 2, 3, 1, 1, 2).count(4) shouldBe 0
  }

  "Multiset" should "correctly add elements" in {
    val set = MultiSet[Int](1, 2, 3)
    set.add(1)
    set.toString shouldBe "[1 -> 2, 2 -> 1, 3 -> 1]"
  }

  "Multiset" should "correctly remove elements" in {
    val set = MultiSet[Int](1, 2, 3, 1)
    set.remove(1)
    set.toString shouldBe "[1 -> 1, 2 -> 1, 3 -> 1]"
    set.remove(1)
    set.toString shouldBe "[2 -> 1, 3 -> 1]"
  }

}