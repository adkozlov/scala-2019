package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class TreapTest extends FlatSpec with Matchers {
  "Insert" should "ok" in {
    new Treap(3, 7).insert(1, 2).insert(4, 1).toList should be(
      List((1, 2), (3, 7), (4, 1))
    )
  }

  "Remove" should "ok" in {
    var treap = new Treap(3, 7).insert(1, 2).insert(4, 1)
    treap.toList should be(List((1, 2), (3, 7), (4, 1)))

    val remove3 = treap.remove(3)
    remove3._1 should be(Some(7))
    treap = remove3._2
    treap.toList should be(List((1, 2), (4, 1)))

    val remove1 = treap.remove(1)
    remove1._1 should be(Some(2))
    treap = remove1._2
    treap.toList should be(List((4, 1)))

    val remove7 = treap.remove(7)
    remove7._1 should be(None)
    treap = remove7._2
    treap.toList should be(List((4, 1)))

    //    treap.remove(4)._1 should be(Some(1))
    //    treap.remove(4)._2.toList should be(List())
    //    treap.toList should be(List())
  }

  "InsertRemove" should "ok" in {
    var treap = new Treap(1, 1).insert(2, 1).insert(3, 1)
    treap.toList should be(List((1, 1), (2, 1), (3, 1)))

    val remove1 = treap.remove(1)
    remove1._1 should be(Some(1))
    treap = remove1._2
    treap.toList should be(List((2, 1), (3, 1)))

    treap = treap.insert(1, 2)
    treap.toList should be(List((1, 2), (2, 1), (3, 1)))
  }
}
