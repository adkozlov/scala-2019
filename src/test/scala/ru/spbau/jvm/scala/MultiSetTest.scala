package ru.spbau.jvm.scala

import org.scalatest.Matchers

class MultiSetTest extends org.scalatest.FlatSpec with Matchers {

  it should "construct the treap" in {
    val set = new MultiSet[Int](1, 2, 1, -1, -1)
    assert(set.toList === List[(Int, Int)](
      (-1, 2),
      (1, 2),
      (2, 1)))
  }

  it should "add distinct values" in {
    val set = new MultiSet[Int]
    for (i <- -100 to 100) {
      set.add(i)
    }
    assert(set.toList === (for (i <- -100 to 100) yield (i, 1)).toList)
  }

  it should "add delta values" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 10) {
      set.add(i, i)
    }

    for (i <- 1 to 10) {
      set.add(i, 2 * i)
    }
    assert(set.toList === (for (i <- 1 to 10) yield (i, 3 * i)).toList)
  }

  it should "not add zero or negative counts" in {
    val set = new MultiSet[Int]
    set.add(2, 0)

    for (i <- 1 to 10) {
      set.add(i, -i)
    }
    assert(set.toList === List.empty)
  }

  it should "return number of keys" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i, i * i)
    }
    for (i <- 1 to 100) {
      assert(set(i) === i * i)
    }
  }

  it should "remove all specific keys" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i, i * i)
    }

    for (i <- 1 to 50) {
      set.removeAll(i)
    }
    assert(set.toList === (for (i <- 51 to 100) yield (i, i * i)).toList)
  }

  it should "remove some of the keys" in {
    val set = new MultiSet[Int]
    for (i <- 2 to 100) {
      set.add(i, i * i)
    }

    for (i <- 2 to 100) {
      set.remove(i, i * i - i)
    }
    assert(set.toList === (for (i <- 2 to 100) yield (i, i)).toList)
  }

  it should "not do anything trying to remove non-existing key" in {
    val set = new MultiSet[Int]
    for (i <- 2 to 100) {
      set.add(i, i * i)
    }

    for (i <- 101 to 110) {
      set.remove(i, i * i - i)
    }
    assert(set.toList === (for (i <- 2 to 100) yield (i, i * i)).toList)
  }

  it should "remove not more keys than exist" in {
    val set = new MultiSet[Int]
    for (i <- 2 to 100) {
      set.add(i, i * i)
    }

    for (i <- 2 to 50) {
      set.remove(i, i * i + i)
    }
    assert(set.toList === (for (i <- 51 to 100) yield (i, i * i)).toList)
  }

  it should "clear the treap" in {
    val set = new MultiSet[Int]
    for (i <- 2 to 100) {
      set.add(i, i * i)
    }
    set.clear()
    assert(set.toList === List.empty)
  }

  it should "unite multisets" in {
    val first = new MultiSet[Int](1, 1, 2, 4)
    val second = new MultiSet[Int](1, 3, 4, 5)
    assert((first | second).toList == List(
      (1, 3),
      (2, 1),
      (3, 1),
      (4, 2),
      (5, 1)
    ))
  }

  it should "intersect multisets" in {
    val first = new MultiSet[Int](1, 1, 2, 4)
    val second = new MultiSet[Int](1, 3, 4, 5)
    assert((first & second).toList == List(
      (1, 1),
      (4, 1)
    ))
  }

  it should "filter elements" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 100) {
      set.add(i, i)
    }
    assert(set.filter(key => key % 2 == 0).toList ===
      (for (i <- 1 to 100 if i % 2 == 0) yield (i, i)).toList)
  }

  it should "map elements to the same type" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 10) {
      set.add(i)
    }
    assert(set.map(key => key % 2).toList === List(
      (0, 5),
      (1, 5)
    ))
  }

  it should "map elements to another type" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 10) {
      set.add(i)
    }
    assert(set.map(key => "" + key % 2).toList === List(
      ("0", 5),
      ("1", 5)
    ))
  }

  it should "perform foreach" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 9) {
      set.add(i)
    }
    var concat = ""
    set.foreach(key => concat += key)
    assert(concat === "123456789")
  }

  it should "work inside for-comprehension" in {
    val set = new MultiSet[Int]
    for (i <- 1 to 9) {
      set.add(i)
    }
    var concat = ""
    for (key <- set if key % 2 == 0) {
      concat += key
    }
    assert(concat === "2468")
  }
}