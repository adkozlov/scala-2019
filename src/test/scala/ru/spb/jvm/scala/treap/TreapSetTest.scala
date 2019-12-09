package ru.spb.jvm.scala.treap

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ru.spbau.jvm.scala.treap.TreapMultiSet

class TreapSetTest extends FunSuite with BeforeAndAfterEach {
  private val seq = Seq(5, 1, 2, 3, 3, 5, 5)
  private def uniqueSeq = seq.distinct
//  override def beforeEach() = {} TODO delete
  test("Constructor does not fail on unique seq") {
    val set = TreapMultiSet(uniqueSeq:_*)
  }

  test("Constructor does not fail on not unique seq") {
    val set = TreapMultiSet(seq:_*)
  }

  test("Count of present element is correct") {
    val set = TreapMultiSet(seq:_*)
    for (elem <- uniqueSeq) {
      assert(seq.count(_ == elem) == set.count(elem))
    }
  }

  test("foreach correctness") {
    val set = TreapMultiSet(seq:_*)
    var res: Seq[Int] = Seq.empty
    for (x <- set) {
      res = res :+ x
    }
    assert(res == seq.sorted)
  }

  test("| adds old correctness") {
    val set = TreapMultiSet(seq:_*)
    val set2 = TreapMultiSet(1, 1, 1)
    val resultSet = set | set2
    assert(3 == resultSet.count(1))
  }

  test("| adds new value correctness") {
    val set = TreapMultiSet(seq:_*)
    val set2 = TreapMultiSet(4, 4)
    val resultSet = set | set2
    assert(2 == resultSet.count(4))
  }

  test("& with existing elements") {
    val set = TreapMultiSet(seq:_*)
    val set2 = TreapMultiSet(5, 5)
    val resultSet = set & set2
    assert(2 == resultSet.count(5))
  }

  test("& with non-existant element") {
    val set = TreapMultiSet(seq:_*)
    val set2 = TreapMultiSet(4, 4)
    val resultSet = set & set2
    assert(0 == resultSet.count(4))
  }

  test("map correctness") {
    val set = TreapMultiSet(seq:_*).map(_ + 1)
    for (elem <- uniqueSeq) {
      assert(seq.count(_ == elem) == set.count(elem + 1))
    }
  }

  test("filter correctness") {
    val set = TreapMultiSet(seq:_*).withFilter(_ > 2)
    for (elem <- uniqueSeq) {
      assert((if (elem > 2) seq.count(_ == elem) else 0) == set.count(elem))
    }
  }

  test("various for-comprehensions compile and give correct type") {
    assert("all good" == (
        (for {
          x <- TreapMultiSet(seq:_*)
          if x > 2
        } yield x.toString) match {
          case _: TreapMultiSet[String] => "all good"
          case _ => "wrong type"
        }
      )
    )
  }

  test("toStringTest") {
    assert("[1 -> 2, 3 -> 1]" == TreapMultiSet(3, 1, 1).toString())
  }
}

object Runner extends App {
  org.scalatest.run(new TreapSetTest())
}
