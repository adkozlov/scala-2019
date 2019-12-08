import org.scalatest.FunSuite
import ru.spbau.jvm.scala.homework02.MultiSet

class MultiSetTest extends FunSuite {
  test("Empty multiset.") {
    val ms = MultiSet.empty[Int]
    assert(ms.size == 0)
  }

  test("Apply multiset.") {
    val ms = MultiSet(1, 1, 2, 1, 2, 3)
    assert(ms.count(1) == 3)
    assert(ms.count(2) == 2)
    assert(ms.count(3) == 1)
    assert(ms.count(4) == 0)
  }

  test("Find in multiset.") {
    val ms = MultiSet(1, 1, 2, 1, 2, 3)
    assert(ms.find(1).get == (1, 3))
    assert(ms.find(2).get == (2, 2))
    assert(ms.find(3).get == (3, 1))
    assert(ms.find(4).isEmpty)
  }

  test("Insert into multiset.") {
    val ms = MultiSet.empty[Int]
    ms.insert(1)
    ms.insert(1)
    ms.insert(2)
    ms.insert(1)
    ms.insert(2)
    ms.insert(3)
    assert(ms.count(1) == 3)
    assert(ms.count(2) == 2)
    assert(ms.count(3) == 1)
    assert(ms.count(4) == 0)
  }


  test("Remove from multiset.") {
    val ms = MultiSet(1, 1, 2, 1, 2, 3)
    assert(ms.count(1) == 3)
    ms.remove(1)
    assert(ms.count(1) == 2)

    assert(ms.count(2) == 2)
    ms.remove(2)
    assert(ms.count(2) == 1)

    assert(ms.count(3) == 1)
    ms.remove(3)
    assert(ms.count(3) == 0)

    assert(ms.count(4) == 0)
    ms.remove(4)
    assert(ms.count(4) == 0)
  }

  test("Union multiset.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(1, 1, 2, 4, 4)
    val ms3 = ms1 | ms2
    assert(ms3.size == ms1.size + ms2.size)
    assert(ms3.count(1) == 5)
    assert(ms3.count(2) == 3)
    assert(ms3.count(3) == 1)
    assert(ms3.count(4) == 2)
  }

  test("Union multiset and submultiset.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(1, 1, 2)
    val ms3 = ms1 | ms2
    assert(ms3.size == ms1.size + ms2.size)
    assert(ms3.count(1) == 5)
    assert(ms3.count(2) == 3)
    assert(ms3.count(3) == 1)
  }

  test("Union different multisets.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(4, 4, 5)
    val ms3 = ms1 | ms2
    assert(ms3.size == ms1.size + ms2.size)
    assert(ms3.count(1) == 3)
    assert(ms3.count(2) == 2)
    assert(ms3.count(3) == 1)
    assert(ms3.count(4) == 2)
    assert(ms3.count(5) == 1)
  }

  test("Intersect multiset.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(1, 1, 2, 4, 5)
    val ms3 = ms1 & ms2
    assert(ms3.size == 3)
    assert(ms3.count(1) == 2)
    assert(ms3.count(2) == 1)
  }

  test("Intersect multiset and submultiset.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(1, 1, 2)
    val ms3 = ms1 & ms2
    assert(ms3.size == ms2.size)
    assert(ms3.count(1) == 2)
    assert(ms3.count(2) == 1)
  }

  test("Intersect different multisets.") {
    val ms1 = MultiSet(1, 1, 2, 1, 2, 3)
    val ms2 = MultiSet(4, 4, 5)
    val ms3 = ms1 & ms2
    assert(ms3.size == 0)
  }
}
