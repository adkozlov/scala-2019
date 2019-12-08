package ru.spb.jvm.scala.treap

import org.scalatest.{BeforeAndAfterEach, FunSuite}
import ru.spbau.jvm.scala.treap.TreapMultiSet

class treapTest extends FunSuite with BeforeAndAfterEach {
  private val seq = Seq(5, 1, 2, 3, 3, 5, 5)
  private def uniqueSeq = seq.distinct
//  override def beforeEach() = {}
  test("Constructor does not fail") {
    val set = new TreapMultiSet(uniqueSeq:_*)
  }
}

object Runner extends App {
  org.scalatest.run(new treapTest())
}