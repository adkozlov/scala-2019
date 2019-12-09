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
}

object Runner extends App {
  org.scalatest.run(new treapSetTest())
}
