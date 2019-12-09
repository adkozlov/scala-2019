package ru.spbau.jvm.scala.multiset

import org.scalatest.FlatSpec

class TestMultiSet extends FlatSpec {
  "Element count " should " be correct in Multiset" in {
    val set = MultiSet(4, 8, 15, 16, 23, 42, 42)
    assert(set(42) === 2)
  }
}
