package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.treap.{Leaf, Node, Treap}

class TreapTest extends FlatSpec with Matchers {
  "SplitAndMerge" should "ok" in {
    val tree =
      Node(3, 7, Node(1, 2, Leaf(), Leaf()), Node(4, 1, Leaf(), Leaf()))
    val splitTrees = Treap.split(3, tree)

    splitTrees should be(
      (
        Node(1, 2, Leaf(), Leaf()),
        Node(3, 7, Leaf(), Node(4, 1, Leaf(), Leaf()))
      )
    )

    Treap.merge(splitTrees._1, splitTrees._2) should be(tree)
  }

  "InsertAndRemove" should "ok" in {
    var tree = Treap.insert(3, 7, Leaf())
    tree = Treap.insert(1, 2, tree)
    tree = Treap.insert(4, 1, tree)
    tree should be(
      Node(3, 7, Node(1, 2, Leaf(), Leaf()), Node(4, 1, Leaf(), Leaf()))
    )

    val remove3 = Treap.remove(3, tree)
    remove3._1 should be(Some(7))
    tree = remove3._2
    tree should be(Node(1, 2, Leaf(), Node(4, 1, Leaf(), Leaf())))

    val remove1 = Treap.remove(1, tree)
    remove1._1 should be(Some(2))
    tree = remove1._2
    tree should be(Node(4, 1, Leaf(), Leaf()))

    val remove7 = Treap.remove(7, tree)
    remove7._1 should be(None)
    tree = remove7._2
    tree should be(Node(4, 1, Leaf(), Leaf()))

    val remove4 = Treap.remove(4, tree)
    remove4._1 should be(Some(1))
    tree = remove4._2
    tree should be(Leaf())
  }

  "InsertAndGetPriByKey" should "ok" in {
    var tree = Treap.insert(3, 7, Leaf())
    tree = Treap.insert(1, 2, tree)
    tree = Treap.insert(4, 1, tree)
    tree = Treap.insert(4, 3, tree)
    tree should be(
      Node(
        3,
        7,
        Node(1, 2, Leaf(), Leaf()),
        Node(4, 3, Leaf(), Node(4, 1, Leaf(), Leaf()))
      )
    )

    Treap.getPriByKey(3, tree) should be(Some(7))
    Treap.getPriByKey(1, tree) should be(Some(2))
    Treap.getPriByKey(4, tree) should be(Some(3))
  }

  "Map" should "ok" in {
    var tree = Treap.insert(3, 7, Leaf())
    tree = Treap.insert(1, 2, tree)
    tree = Treap.insert(4, 1, tree)

    Treap.toString(Treap.map((x: Int) => if (x == 1) 5 else x, tree)) should be(
      " 3->7  5->2  4->1 "
    )
  }
}
