package ru.spbau.jvm.scala.multiset

import org.scalatest.{FlatSpec, Matchers}

class TreapTest extends FlatSpec with Matchers {

  private def constructTreap() = {
    val treap = new Treap[Int, Integer]
    treap.add(11, 1)
    treap.add(21, 2)
    treap.add(31, 3)
    treap.add(41, 4)
    treap
  }

  "Treap" should "contains elems after add" in {
    val treap = constructTreap()
    treap.get(11) should be(1)
    treap.get(21) should be(2)
    treap.get(31) should be(3)
    treap.get(41) should be(4)
  }

  "Treap" should "change value after double adds" in {
    val treap = constructTreap()
    treap.add(10, 10)
    treap.add(10, 11)
    treap.get(10) should be(11)
  }

  "Treap" should "not contains elems after remove" in {
    val treap = constructTreap()
    treap.remove(41)
    treap.get(41) should be(null)
  }

  "Treap" should "return last changed value" in {
    val treap = constructTreap()
    treap.changeValue(41, 10)
    treap.get(41) should be(10)
  }

  "Treap" should "have correct size after adds" in {
    val treap = constructTreap()
    treap.size() should be(4)
  }

  "Treap" should "have correct size after remove" in {
    val treap = constructTreap()
    treap.remove(41)
    treap.remove(21)
    treap.size() should be(2)
  }

  "Treap toList" should "be correct" in {
    val treap = constructTreap()
    treap.toList should be((11, 1) :: (21, 2) :: (31, 3) :: (41, 4) :: Nil)
  }
}
