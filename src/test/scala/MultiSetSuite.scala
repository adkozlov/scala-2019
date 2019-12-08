import org.scalatest.FunSuite
import ru.spbau.jvm.scala.multiset.MultiSet

class MultiSetSuite extends FunSuite {
  test("Empty set.") {
    val ms = MultiSet[Int]
    assert(ms.toString == "[]")
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
    val ms = MultiSet[Int]
    ms.add(3)
    assert(ms.getCount(3) == 1)
    ms.add(3)
    assert(ms.getCount(3) == 2)
    ms.add(3, 2)
    assert(ms.getCount(3) == 4)
  }

  test("Test remove") {
    val ms = MultiSet[Int]
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
    assert((multiSet & MultiSet(42)).toString == "[42->1]")
    assert((multiSet & MultiSet(43, 43, 43)).toString == "[]")
    assert((multiSet & MultiSet(42, 42, 42)).toString == "[42->2]")
    assert((multiSet & multiSet).toString == "[4->1,8->1,15->1,16->1,23->1,42->2]")
  }

  test("Test unite.") {
    val multiSet = MultiSet(4, 8, 15, 16, 23, 42, 42)
    assert((multiSet | MultiSet(42)).toString == "[4->1,8->1,15->1,16->1,23->1,42->3]")
    assert((multiSet | MultiSet(43, 43, 43)).toString == "[4->1,8->1,15->1,16->1,23->1,42->2,43->3]")
    assert((multiSet | MultiSet(42, 42, 42)).toString == "[4->1,8->1,15->1,16->1,23->1,42->5]")
    assert((multiSet | multiSet).toString == "[4->2,8->2,15->2,16->2,23->2,42->4]")
  }

  test("Test foreach.") {
    var sum = 0
    val multiSet = MultiSet(1, 2, 3, 4)
    multiSet.foreach(x => sum += x)
    assert(sum == 10)
  }

  test("Test filter.") {
    val multiSet = MultiSet(1, 2, 3, 4)
    assert(multiSet.filter(x => x % 2 == 0).toString() == "[2->1,4->1]")
  }

  test("Test map.") {
    val multiSet = MultiSet(1, 2)
    assert(multiSet.map(x => x.toString + "a").toString() == "[1a->1,2a->1]")
  }

  test("Test get count.") {
    val ms = MultiSet(1, 2, 3, 4)
    assert(ms(1) == 1)
    assert(ms(2) == 1)
    assert(ms(3) == 1)
    assert(ms(4) == 1)
  }

}