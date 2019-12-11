package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.treap.{Leaf, Treap, Tree}

object MultiSet {
  def empty[E]()(implicit ordering: Ordering[E]) =
    new MultiSet[E](Leaf())

  def apply[E](elems: E*)(implicit ord: Ordering[E]): MultiSet[E] = {
    val ms = empty[E]()
    for (elem <- elems) {
      ms.insert(elem)
    }
    ms
  }
}

class MultiSet[E](private var tree: Tree[E, Int] = Leaf())(
  implicit ord: Ordering[E]
) {

  def insert(elem: E): MultiSet[E] = insert(elem, 1)

  def insert(elem: E, count: Int): MultiSet[E] = {
    if (count > 0) {
      val (opPri, newTree) = Treap.remove(elem, tree)
      tree = Treap.insert(elem, count + opPri.getOrElse(0), newTree)
    }
    this
  }

  def remove(elem: E): MultiSet[E] = {
    val (opPri, newTree) = Treap.remove(elem, tree)
    val pri = opPri.map(x => x - 1).getOrElse(-1)

    if (pri > 0) {
      tree = Treap.insert(elem, pri, newTree)
    }
    this
  }

  def isEmpty: Boolean = tree == Leaf()

  def notEmpty: Boolean = !isEmpty

  def countOf(elem: E): Int = {
    Treap.getPriByKey(elem, tree).getOrElse(0)
  }

  def contains(elem: E): Boolean = countOf(elem) > 0

  def &(multiSet: MultiSet[E]): MultiSet[E] = {
    val intersect = new MultiSet[E](Leaf())
    Treap.foreach((key: E, pri: Int) => {
      val count = pri.min(multiSet.countOf(key))
      intersect.insert(key, count)
    }, tree)
    intersect
  }

  def |(multiSet: MultiSet[E]): MultiSet[E] = {
    val union = new MultiSet[E](tree)
    multiSet.foreach(key => union.insert(key, multiSet.countOf(key)))
    union
  }

  def foreach[R](f: E => R): Unit = Treap.foreachKey(f, tree)

  def map[R](f: E => R)(implicit ord: Ordering[R]): MultiSet[R] =
    new MultiSet[R](Treap.map(f, tree))

  def filter(p: E => Boolean): MultiSet[E] =
    new MultiSet[E](Treap.filter(p, tree))

  override def toString: String = {
    "[" ++ Treap.toString(tree) ++ "]"
  }
}
