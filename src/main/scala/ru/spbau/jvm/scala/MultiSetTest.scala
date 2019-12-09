package ru.spbau.jvm.scala

import org.scalatest._ // should be enabled in IntelliJ IDEA project structure as dependency in compile mode

class MultiSetTest extends FlatSpec with Matchers {

  "MultiSet" should "be contructed correctly" in
    assert(
      new MultiSet(1, 2, 2, 3, 3, 3).toList == List((1, 1), (2, 2), (3, 3))
    )

  it should "work as set" in {
    val set = new MultiSet[Int]
    for (i <- 0 to 100) {
      set.add(i)
    }
    assert(set.toList == (for (i <- 0 to 100) yield (i, 1)).toList)
  }

  it should "work as MultiSet" in {
    val set = new MultiSet[Int]
    for (_ <- 1 to 2;
         i <- 1 to 100) {
      set.add(i, i)
    }
    assert(set.toList == (for (i <- 1 to 100) yield (i, 2 * i)).toList)
  }

  "apply" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 10) {
      set.add(i, i * i)
    }
    for (i <- 1 to 10) {
      assert(set(i) == i * i)
    }
  }

  "removeAll" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 10) {
      set.add(i, i * i)
    }

    for (i <- 1 to 9) {
      set.removeAll(i)
    }
    assert(set.toList == List((10, 10 * 10)))
  }

  "remove" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 49) {
      set.add(i, 2 * i * i)
    }

    set.add(50, 50 * 50)

    for (i <- 51 to 100) {
      set.add(i, i * i / 2)
    }

    for (i <- 1 to 200) {
      set.remove(i, i * i)
    }
    assert(set.toList == (for (i <- 1 to 49) yield (i, i * i)).toList)
  }

  "clear" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i, i * i)
    }
    set.clear()
    assert(set.toList == List())
  }

  "&" should "work correctly" in {
    val a = new MultiSet[Int](1, 2, 3, 3, 4)
    val b = new MultiSet[Int](1, 3, 5)
    assert((a & b).toList == List((1, 1), (3, 1)))
  }

  "|" should "work correctly" in {
    val a = new MultiSet[Int](1, 2, 3, 4, 4)
    val b = new MultiSet[Int](1, 3, 3, 5)
    assert((a | b).toList == List((1, 2), (2, 1), (3, 3), (4, 2), (5, 1)))
  }

  "filter" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i, i)
    }
    assert(
      set.filter(_ % 2 == 0).toList == (for (i <- 1 to 100 if i % 2 == 0)
        yield (i, i)).toList
    )
  }

  "map" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i)
    }
    assert(set.map(key => key % 2).toList == List((0, 50), (1, 50)))
    assert(
      set.map(key => (key % 2).toString()).toList == List(("0", 50), ("1", 50))
    )
    assert(
      set.map(key => (key % 2) * 0.5f).toList == List((0f, 50), (0.5f, 50))
    )
  }

  "foreach" should "work correctly" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 9) {
      set.add(i)
    }
    var concat = ""
    set.foreach(key => concat += key)
    assert(concat.equals("123456789"))
  }

  "for-comprehension" should "be able" in {
    val set = new MultiSet[Int]
    for (i <- -4 to 2) {
      set.add(i)
    }
    var concat = ""
    for (key <- set if key % 2 == 0) {
      concat += key
    }
    assert(concat.equals("-4-202"))
  }
}
