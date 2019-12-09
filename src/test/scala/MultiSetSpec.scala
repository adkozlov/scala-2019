import java.util

import org.scalatest.Matchers
import ru.spbau.jvm.scala.multiset.MultiSet

class MultiSetSpec extends org.scalatest.FlatSpec with Matchers {
  val defaultElements: List[BigInt] = List(1, 0, 1, 1, 3, 5, 2, 4, 5, 4, 5)
  val sortedElements: Map[BigInt, BigInt] = Map((0:BigInt) -> (1:BigInt), (1:BigInt) -> (3:BigInt), (3:BigInt) -> (1:BigInt), (4:BigInt) -> (2:BigInt), (5:BigInt) -> (3:BigInt))

  def assertElementsEquals(set: MultiSet[BigInt], elements: List[BigInt], mapElements: Map[BigInt, BigInt]): Unit = {
    assert(set.getSize == elements.size)
    mapElements.foreach(elem => assert(set.apply(elem._1) == elem._2, elem._1))
  }

  "Empty multiset" should "be empty" in {
    val set = new MultiSet[BigInt]()
    assert(set.empty())
  }

  "Empty set of one element" should "contain this element" in {
    val set = new MultiSet[BigInt](5)
    assert(set.getSize == 1)
    assert(set.apply(5) == 1)
  }

  "Set of collection" should "contain all elements of this collection" in {
    val set = new MultiSet[BigInt](defaultElements)
    assertElementsEquals(set, defaultElements, sortedElements)
  }

  "Add to set" should "add elements" in {
    val set = new MultiSet[BigInt]()

    set.add(5)
    assert(set.getSize == 1)
    assert(set.apply(5) == 1)

    set.add(5)
    assert(set.getSize == 2)
    assert(set.apply(5) == 2)

    set.add(3)
    assert(set.getSize == 3)
    assert(set.apply(3) == 1)

    set.add(6)
    assert(set.getSize == 4)
    assert(set.apply(6) == 1)

    set.add(6)
    assert(set.getSize == 5)
    assert(set.apply(6) == 2)
  }

  "Remove" should "remove elements from set" in {
    val set = new MultiSet[BigInt](1, 2, 2, 3)

    set.remove(2)
    assert(set.getSize == 3)
    assert(set.apply(2) == 1)

    set.remove(2)
    assert(set.getSize == 2)
    assert(set.apply(2) == 0)

    set.remove(1)
    assert(set.getSize == 1)
    assert(set.apply(1) == 0)

    set.remove(3)
    assert(set.getSize == 0)
    assert(set.apply(3) == 0)
  }

  "Remove not existing element" should "do nothing" in {
    val set = new MultiSet[BigInt](defaultElements)

    set.remove(1000)
    assertElementsEquals(set, defaultElements, sortedElements)
  }

  "Contains" should "return correct result" in {
    val set = new MultiSet[BigInt](defaultElements)

    assert(set.contains(0))
    assert(set.contains(1))
    assert(set.contains(2))
    assert(set.contains(3))
    assert(set.contains(4))
    assert(set.contains(5))

    assert(!set.contains(-1))
    assert(!set.contains(6))

    val emptySet = new MultiSet[BigInt]()
    assert(!emptySet.contains(0))
    assert(!emptySet.contains(1))
  }

  "Apply" should "return correct result" in {
    val set = new MultiSet[BigInt](defaultElements)

    assert(set.apply(0) == 1)
    assert(set.apply(1) == 3)
    assert(set.apply(2) == 1)
    assert(set.apply(3) == 1)
    assert(set.apply(4) == 2)
    assert(set.apply(5) == 3)

    assert(set.apply(-1) == 0)
    assert(set.apply(6) == 0)

    val emptySet = new MultiSet[BigInt]()
    assert(emptySet.apply(0) == 0)
    assert(emptySet.apply(1) == 0)
  }

  "Empty" should "return correct result" in {
    val set = new MultiSet[BigInt]()

    assert(set.empty())
    set.add(1)
    assert(!set.empty())
    set.remove(1)
    assert(set.empty())
  }

  "Intersec of different collections" should "return empty collection" in {
    val set1 = new MultiSet[BigInt](1, 2, 2, 3)
    val set2 = new MultiSet[BigInt](0, 4, 5, 5, 6)

    assert((set1 & set2).empty())
  }

  "Intersec of collections" should "return intersection" in {
    val set1 = new MultiSet[BigInt](1, 2, 2, 3)
    val set2 = new MultiSet[BigInt](3, 0, 2, 2, 2, 4)

    val result = set1 & set2
    assert(result.getSize == 3)
    assert(result.apply(2) == 2)
    assert(result.apply(3) == 1)
  }

  "Intersec of collections with itself" should "return this collection" in {
    val set1 = new MultiSet[BigInt](1, 2, 2, 3)

    val result = set1 & set1
    assert(result.getSize == 4)
    assert(result.apply(1) == 1)
    assert(result.apply(2) == 2)
    assert(result.apply(3) == 1)
  }

  "Union of different collections" should "return union" in {
    val set1 = new MultiSet[BigInt](1, 2, 2, 3)
    val set2 = new MultiSet[BigInt](4, 5, 6, 6)

    val result = set1 | set2
    assert(result.getSize == 8)
    assert(result.apply(1) == 1)
    assert(result.apply(2) == 2)
    assert(result.apply(3) == 1)
    assert(result.apply(4) == 1)
    assert(result.apply(5) == 1)
    assert(result.apply(6) == 2)
  }

  "Union of collection with itself" should "double it" in {
    val set1 = new MultiSet[BigInt](1, 2, 2, 3)

    val result = set1 | set1
    assert(result.getSize == 8)
    assert(result.apply(1) == 2)
    assert(result.apply(2) == 4)
    assert(result.apply(3) == 2)
  }

  "Foreach of empty collection" should "do nothing" in {
    val set = new MultiSet[BigInt]()

    set.foreach(value => assert(false))
  }

  "Foreach of collection" should "be ordered" in {
    val set = new MultiSet[BigInt](1, 2, 0, 2, 3, 6, 0)

    var result: String = ""

    set.foreach(value => {
      result += value + " "
    })

    assert(result == "0 1 2 3 6 ")
  }

  "Map on empty collection" should "do nothing" in {
    val set = new MultiSet[BigInt]()

    set.map(value => value + 1)
    assert(set.empty())
  }

  "Map on collection" should "map" in {
    val set = new MultiSet[BigInt](defaultElements)
    val mapset = set.map(value => value + 1)

    assert(mapset.getSize == defaultElements.size)
    assert(mapset.apply(1) == 1)
    assert(mapset.apply(2) == 3)
    assert(mapset.apply(4) == 1)
    assert(mapset.apply(5) == 2)
    assert(mapset.apply(6) == 3)
  }

  "WithFilter on empty collection" should "do nothing" in {
    val set = new MultiSet[BigInt]()

    set.withFilter(value => true)
    assert(set.empty())
  }

  "WithFilter on collection" should "filer" in {
    val set = new MultiSet[BigInt](defaultElements)
    val filtered = set.withFilter(value => value > 2)

    assert(filtered.getSize == defaultElements.size - 5)
    assert(filtered.apply(3) == 1)
    assert(filtered.apply(4) == 2)
    assert(filtered.apply(5) == 3)
  }

  "WithFilter with emptyfilter" should "return empty collection" in {
    val set = new MultiSet[BigInt](defaultElements)
    val filtered = set.withFilter(value => false)

    assert(filtered.empty())
  }

  "ToString on empty collection" should "return empty collection string" in {
    val set = new MultiSet[BigInt]()
    assert(set.toString == "[]")
  }

  "ToString on collection" should "return collection string" in {
    val set = new MultiSet[BigInt](defaultElements)
    assert(set.toString == "[0 -> 1, 1 -> 3, 2 -> 1, 3 -> 1, 4 -> 2, 5 -> 3]")
  }

  "For-Comprehension on collection" should "work correctly" in {
    val set = new MultiSet[BigInt](1, 2, 1, 1, 4, 5)
    var result = 0
    for (
      value: BigInt <- set
      if (value == 1)
    ) {
      result += 1
    }

    assert(result == 1)
  }
}