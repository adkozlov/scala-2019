package ru.spbau.jvm.scala.MultiSet

import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class MultiSetSpec extends org.scalatest.FlatSpec with Matchers {
  private def toList[T <: Ordered[T]](multiSet: MultiSet[T]): List[T] = {
    val list = new ListBuffer[T]
    for (value <- multiSet) {
      list.addOne(value)
    }
    list.toList
  }

  it should "initialize set" in {
    toList(new MultiSet[BigInt](239, 42, 179)) should be (List(42, 179, 239))
  }

  it should "initialize set with duplicated values" in {
    toList(new MultiSet[BigInt](42, 239, 42, 179)) should be (List(42, 179, 239))
  }

  it should "count occurrences" in {
    val multiSet = new MultiSet[BigInt](42, 239, 42, 179)
    multiSet(42) should be (2)
    multiSet(239) should be (1)
    multiSet(1337) should be(0)
  }

  it should "add elements" in {
    val multiSet = new MultiSet[BigInt](42, 239, 42, 179)
    multiSet.put(42)
    multiSet(42) should be (3)
    multiSet.put(1337)
    multiSet(1337) should be(1)
    toList(multiSet) should be (List(42, 179, 239, 1337))
  }

  it should "remove elements" in {
    val multiSet = new MultiSet[BigInt](42, 239, 42, 179)
    multiSet.remove(179) should be (1)
    toList(multiSet) should be (List(42, 239))
    multiSet.remove(42) should be (2)
    toList(multiSet) should be (List(239))
    multiSet.put(43)
    multiSet.put(43)
    multiSet.put(43)
    multiSet.remove(43, 2) should be (2)
    multiSet.remove(178) should be (0)
    multiSet.remove(43, 10) should be (1)
  }

  "size" should "work" in {
    val multiSet = new MultiSet[BigInt](2, 42, 42)
    multiSet.size() should be (2)
    multiSet.put(2)
    multiSet.size() should be (2)
    multiSet.put(3)
    multiSet.size() should be (3)
    multiSet.remove(42)
    multiSet.size() should be (2)
  }

  it should "work on empty MultiSet" in {
    val multiSet = new MultiSet[BigInt]()
    multiSet(42) should be (0)
    multiSet.remove(43) should be (0)
    toList(multiSet) should be (List.empty)
    multiSet.size() should be (0)
    multiSet.toString should be ("[]")
    multiSet.firstValue() should be (None)
    multiSet.lastValue() should be (None)
    multiSet.put(42)
    toList(multiSet) should be (List(42))
  }

  "toString" should "work" in {
    val multiSet = new MultiSet[BigInt](239, 179, 42, 42)
    multiSet.toString should be ("[42 -> 2, 179 -> 1, 239 -> 1]")
  }

  "intersection" should "work" in {
    val int1 = new MultiSet[BigInt](42, 42, 239) & new MultiSet[BigInt](31, 42)
    int1.toString should be ("[42 -> 3]")
    val int2 = new MultiSet[BigInt](42, 42, 239) & new MultiSet[BigInt](31)
    int2.toString should be ("[]")
    val int3 = new MultiSet[BigInt](42, 42, 239) & new MultiSet[BigInt]()
    int3.toString should be ("[]")
    val int4 = new MultiSet[BigInt]() & new MultiSet[BigInt]()
    int4.toString should be ("[]")
  }

  "union" should "work" in {
    val uni1 = new MultiSet[BigInt](42, 42, 239) | new MultiSet[BigInt](31, 42)
    uni1.toString should be ("[31 -> 1, 42 -> 3, 239 -> 1]")
    val uni2 = new MultiSet[BigInt](42, 42, 239) | new MultiSet[BigInt](31)
    uni2.toString should be ("[31 -> 1, 42 -> 2, 239 -> 1]")
    val uni3 = new MultiSet[BigInt](42, 42, 239) | new MultiSet[BigInt]()
    uni3.toString should be ("[42 -> 2, 239 -> 1]")
    val uni4 = new MultiSet[BigInt]() | new MultiSet[BigInt]()
    uni4.toString should be ("[]")
  }

  "firstValue and lastValue" should "work" in {
    val multiSet = new MultiSet[BigInt](42, 17, 239)
    multiSet.firstValue() should be (Some(17))
    multiSet.lastValue() should be (Some(239))
  }

  it should "throw on incorrect arguments" in {
    val multiSet = new MultiSet[BigInt](42, 17, 239)
    assertThrows[IllegalArgumentException](multiSet.remove(42, -1))
    assertThrows[IllegalArgumentException](multiSet.remove(null))
    assertThrows[IllegalArgumentException](multiSet.put(null))
    assertThrows[IllegalArgumentException](multiSet(null))
  }

  "for-comprehension" should "work" in {
    toList(new MultiSet[BigInt](239, 42)) should be (List(42, 239))

    var sum: BigInt = 0
    for {
      i <- new MultiSet[BigInt](239, 42, -1)
      if i > 0
    } {
      sum += i
    }
    sum should be (239 + 42)
  }

  "map" should "work" in {
    implicit class OrderedString(val s: String) extends Ordered[OrderedString] {
      override def compare(that: OrderedString): Int = s.compareTo(that.s)

      override def toString: String = s

      override def equals(obj: Any): Boolean = {
        if (obj.getClass != this.getClass) {
          return false
        }
        obj.asInstanceOf[OrderedString].s == s
      }
    }

    val newMultiSet = new MultiSet[BigInt](42, 239, 8)
      .map[OrderedString](x => x.toString())
    toList(newMultiSet) should be (List[OrderedString]("239", "42", "8"))
  }

  "filter" should "work" in {
    val multiSet = new MultiSet[BigInt](42, -1, -1, 3, 5, 42)
    multiSet.filter(x => x > 0).toString should be ("[3 -> 1, 5 -> 1, 42 -> 2]")
  }

  "samples" should "work" in {
    val multiSet = new MultiSet[BigInt](4, 8, 15, 16, 23, 42, 42)
    multiSet.toString should be ("[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2]")
    multiSet(42) should be (2)
    (multiSet & new MultiSet(42)).toString should be ("[42 -> 3]")
    (multiSet | new MultiSet(108)).toString should be (
      "[4 -> 1, 8 -> 1, 15 -> 1, 16 -> 1, 23 -> 1, 42 -> 2, 108 -> 1]")
  }
}
