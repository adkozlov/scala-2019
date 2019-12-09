package ru.spb.jvm.scala.treap

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ru.spbau.jvm.scala.treap.TreapMultiSet

class treapSetTest extends FunSuite with BeforeAndAfterEach {
  private val seq = Seq(5, 1, 2, 3, 3, 5, 5)
  private def uniqueSeq = seq.distinct
//  override def beforeEach() = {} TODO delete
  test("Constructor does not fail on unique seq") {
    val set = new TreapMultiSet(uniqueSeq:_*)
  }

  test("Constructor does not fail on not unique seq") {
    val set = new TreapMultiSet(seq:_*)
  }

  test("Count of present element is correct") {
    val set = new TreapMultiSet(seq:_*)
    for (elem <- uniqueSeq) {
      assert(seq.count(_ == elem) == set.count(elem))
    }
  }

  test("foreach correctness") {
    val set = new TreapMultiSet(seq:_*)
    var res: Seq[Int] = Seq.empty
    for (x <- set) {
      res = res :+ x
    }
    assert(res == seq.sorted)
  }

  test("| adds old correctness") {
    val set = new TreapMultiSet(seq:_*)
    val set2 = new TreapMultiSet(1, 1, 1)
    val resultSet = set | set2
    assert(3 == resultSet.count(1))
  }

  test("| adds new value correctness") {
    val set = new TreapMultiSet(seq:_*)
    val set2 = new TreapMultiSet(4, 4)
    val resultSet = set | set2
    assert(2 == resultSet.count(4))
  }
}

object Runner extends App {
  org.scalatest.run(new treapSetTest())
}
