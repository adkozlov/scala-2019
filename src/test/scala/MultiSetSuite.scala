import org.scalatest.FunSuite
import ru.spbau.jvm.scala.multiset.MultiSet

class MultiSetSuite extends FunSuite {
  test("Empty set.") {
    val ms = MultiSet[Int]()
    assert(ms.isEmpty())
  }

  test("Non empty set.") {
    val ms = MultiSet(1, 2, 3, 4)
    assert(ms.getCount(1) == 1)
    assert(ms.getCount(2) == 1)
    assert(ms.getCount(3) == 1)
    assert(ms.getCount(4) == 1)
  }

  test("Repeated value set.") {
    val ms = MultiSet(1, 2, 1, 2)
    assert(ms.getCount(1) == 2)
    assert(ms.getCount(2) == 2)
  }

  test("Test add.") {
    val ms = MultiSet[Int]()
    ms.add(3)
    assert(ms.getCount(3) == 1)
    ms.add(3)
    assert(ms.getCount(3) == 2)
    ms.add(3, 2)
    assert(ms.getCount(3) == 4)
  }

  test("Test remove") {
    val ms = MultiSet[Int]()
    assert(ms.getCount(3) == 0)
    ms.remove(3)
    assert(ms.getCount(3) == 0)
    ms.add(3, 2)
    assert(ms.getCount(3) == 2)
    ms.remove(3)
    assert(ms.getCount(3) == 1)
  }

  test("Test contains") {
    val ms = MultiSet(1, 2, 3, 4)
    assert(ms.contains(1))
    assert(ms.contains(2))
    assert(ms.contains(3))
    assert(ms.contains(4))
    ms.remove(2)
    assert(!ms.contains(2))
  }

  test("Test find.") {
    val ms = MultiSet(1, 2, 3, 4, 2, 2)
    val data = ms.findData(2)
    assert(data.isDefined)
    assert(data.get.key == 2)
    assert(data.get.count == 3)
  }

  test("Test iterator.") {
    val multiSet = MultiSet(2, 2, 1, 1, 3, 4, 3, 4)
    val it = multiSet.iterator()
    assert(it.hasNext); assert(it.next() == 1)
    assert(it.hasNext); assert(it.next() == 1)
    assert(it.hasNext); assert(it.next() == 2)
    assert(it.hasNext); assert(it.next() == 2)
    assert(it.hasNext); assert(it.next() == 3)
    assert(it.hasNext); assert(it.next() == 3)
    assert(it.hasNext); assert(it.next() == 4)
    assert(it.hasNext); assert(it.next() == 4)
  }

  test("Test intersect.") {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)

    val ms1 = multiSet & MultiSet(42)
    assert(ms1(42) == 1)

    assert((multiSet & MultiSet(43, 43, 43)).isEmpty())

    val ms2 = multiSet & MultiSet(42, 42, 42)
    assert(ms2(42) == 2)

    val ms3 = multiSet & multiSet
    assert(ms3(4) == 1)
    assert(ms3(8) == 1)
    assert(ms3(15) == 1)
    assert(ms3(16) == 1)
    assert(ms3(23) == 1)
    assert(ms3(42) == 2)
  }

  test("Test unite.") {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)

    val ms1 = multiSet | MultiSet(42)
    assert(ms1(4) == 1)
    assert(ms1(8) == 1)
    assert(ms1(15) == 1)
    assert(ms1(16) == 1)
    assert(ms1(23) == 1)
    assert(ms1(42) == 3)

    val ms2 = multiSet | MultiSet(43, 43, 43)
    assert(ms2(4) == 1)
    assert(ms2(8) == 1)
    assert(ms2(15) == 1)
    assert(ms2(16) == 1)
    assert(ms2(23) == 1)
    assert(ms2(42) == 2)
    assert(ms2(43) == 3)

    val ms3 = multiSet | MultiSet(42, 42, 42)
    assert(ms3(4) == 1)
    assert(ms3(8) == 1)
    assert(ms3(15) == 1)
    assert(ms3(16) == 1)
    assert(ms3(23) == 1)
    assert(ms3(42) == 5)

    val ms4 = multiSet | multiSet
    assert(ms4(4) == 2)
    assert(ms4(8) == 2)
    assert(ms4(15) == 2)
    assert(ms4(16) == 2)
    assert(ms4(23) == 2)
    assert(ms4(42) == 4)
  }

  test("Test foreach.") {
    var sum = 0
    val multiSet = MultiSet(1, 2, 3, 4)
    multiSet.foreach(x => sum += x)
    assert(sum == 10)
  }

  test("Test filter.") {
    val multiSet = MultiSet(1, 2, 3, 4)
    val ms = multiSet.filter(x => x % 2 == 0)
    assert(ms(2) == 1)
    assert(ms(4) == 1)
  }

  test("Test map.") {
    val multiSet = MultiSet(1, 2)
    val ms = multiSet.map(x => x.toString + "a")
    assert(ms.toList == List("1a", "2a"))
  }

  test("Test get count.") {
    val ms = MultiSet(1, 2, 3, 4)
    assert(ms(1) == 1)
    assert(ms(2) == 1)
    assert(ms(3) == 1)
    assert(ms(4) == 1)
  }

}