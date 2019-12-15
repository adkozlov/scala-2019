package ru.spbau.jvm.scala

import org.scalatest.FunSuite

class AvlTreeTest extends FunSuite {
  test("empty test") {
    val treeEmpty = AvlNil[Int]()
    assert(treeEmpty.depth == 0)
    assert(treeEmpty.max.isEmpty)
    assert(treeEmpty.min.isEmpty)
  }

  test("simple test") {
    val treeEmpty = AvlNil[Int]()
    val tree = treeEmpty.add(1).add(2).add(3).add(4).add(5)
    val treeResult = AvlNode(3, 1, 2,
      AvlNode(1, 1, 1, AvlNil[Int](), AvlNil[Int]()),
      AvlNode(2, 1, 4,
        AvlNode(1, 1, 3, AvlNil[Int](), AvlNil[Int]()),
        AvlNode(1, 1, 5, AvlNil[Int](), AvlNil[Int]())))
    assert(tree == treeResult)
    assert(tree.max.contains((5, 1)))
    assert(tree.min.contains((1, 1)))
    tree.balance match {
      case isBalanced() =>
      case _ => fail()
    }

    for (elem <- 1 until 5) {
      assert(tree.contains(elem))
    }
  }

  test("remove test") {
    val treeEmpty = AvlNil[Int]()
    val tree = treeEmpty.add(1).add(2).add(3).add(4).add(5).remove(2)
    tree.balance match {
      case isBalanced() =>
      case _ => fail()
    }

    val treeResult = AvlNode(3, 1, 4,
      AvlNode(2, 1, 1, AvlNil[Int](), AvlNode(1, 1, 3, AvlNil[Int](), AvlNil[Int]())),
      AvlNode(1, 1, 5, AvlNil[Int](), AvlNil[Int]()))

    assert(tree == treeResult)

    assert(tree.contains(1))
    assert(tree.contains(3))
    assert(tree.contains(4))
    assert(tree.contains(5))
  }
}
