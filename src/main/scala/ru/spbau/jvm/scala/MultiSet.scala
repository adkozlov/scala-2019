package ru.spbau.jvm.scala

object MultiSet {
  def empty[E]()(implicit ordering: Ordering[E]) =
    new MultiSet[E]

  def apply[E](elems: E*)(implicit ord: Ordering[E]): MultiSet[E] = {
    val ms = empty[E]()
    for (elem <- elems) {
      ms.insert(elem)
    }
    ms
  }
}

class MultiSet[K](implicit ord: Ordering[K]) {
  private var tree: Tree = Leaf

  def insert(elem: K): MultiSet[K] = insert(elem, 1)

  def insert(elem: K, count: Int): MultiSet[K] = {
    if (count > 0) {
      val (opPri, newTree) = remove(elem, tree)
      tree = insert(Node(elem, count + opPri.getOrElse(0)), newTree)
    }
    this
  }

  def remove(elem: K): MultiSet[K] = {
    val (opPri, newTree) = remove(elem, tree)
    val pri = opPri.map(x => x - 1).getOrElse(-1)

    if (pri > 0) {
      tree = insert(Node(elem, pri), newTree)
    }
    this
  }

  def isEmpty: Boolean = tree == Leaf

  def notEmpty: Boolean = !isEmpty

  def countOf(elem: K): Int = {
    getCountByKey(elem, tree).getOrElse(0)
  }

  def contains(elem: K): Boolean = countOf(elem) > 0

  def foreach[R](f: K => R): Unit = foreachKey(f, tree)

  def &(multiSet: MultiSet[K]): MultiSet[K] = {
    val intersect = new MultiSet[K]
    foreach((key: K, pri: Int) => {
      val count = pri.min(multiSet.countOf(key))
      intersect.insert(key, count)
    }, tree)
    intersect
  }

  def |(multiSet: MultiSet[K]): MultiSet[K] = {
    val union = new MultiSet[K]
    foreach(key => union.insert(key, countOf(key)))
    multiSet.foreach(key => union.insert(key, multiSet.countOf(key)))
    union
  }

  def map[R](f: K => R)(implicit ord: Ordering[R]): MultiSet[R] = {
    val set = new MultiSet[R]
    foreach(key => set.insert(f(key), countOf(key)))
    set
  }

  def filter(p: K => Boolean): MultiSet[K] = {
    val set = new MultiSet[K]
    foreach(key => if (p(key)) set.insert(key, countOf(key)))
    set
  }

  override def toString: String = toString(tree)

  override def equals(other: Any): Boolean = {
    other match {
      case tree: MultiSet[K] => {
        for (key <- this) {
          if (tree.countOf(key) != countOf(key)) {
            return false
          }
        }
        for (key <- tree) {
          if (tree.countOf(key) != countOf(key)) {
            return false
          }
        }
        true
      }
      case _ => false
    }
  }

  private abstract class Tree

  private case class Node(key: K,
                          pri: Int,
                          left: Tree = Leaf,
                          right: Tree = Leaf)
      extends Tree

  private case object Leaf extends Tree

  private def split(splitKey: K, tree: Tree): (Tree, Tree) = {
    tree match {
      case Leaf => (Leaf, Leaf)
      case Node(key: K, pri, left: Tree, right: Tree) =>
        if (ord.lt(key, splitKey)) {
          val (t1, t2) = split(splitKey, right)
          (Node(key, pri, left, t1), t2)
        } else {
          val (t1, t2) = split(splitKey, left)
          (t1, Node(key, pri, t2, right))
        }
    }
  }

  private def merge(tree1: Tree, tree2: Tree): Tree = {
    (tree1, tree2) match {
      case (Leaf, _) => tree2
      case (_, Leaf) => tree1
      case (Node(key1, pri1, left1, right1), Node(key2, pri2, left2, right2)) =>
        if (pri1 < pri2) {
          val t: Tree = merge(tree1, left2)
          Node(key2, pri2, t, right2)
        } else {
          val t: Tree = merge(right1, tree2)
          Node(key1, pri1, left1, t)
        }
    }
  }

  private def insert(node: Tree, tree: Tree): Tree = {
    node match {
      case Leaf => tree
      case Node(key, _, _, _) =>
        val (t1, t2) = split(key, tree)
        merge(merge(t1, node), t2)
    }
  }

  private def remove(removeKey: K, tree: Tree): (Option[Int], Tree) = {
    tree match {
      case Leaf => (None, Leaf)
      case Node(key: K, pri, left, right) =>
        if (ord.compare(key, removeKey) == 0) {
          return (Some(pri), merge(left, right))
        }

        if (ord.lt(key, removeKey)) {
          val (opPri, newRight) = remove(removeKey, right)
          (opPri, Node(key, pri, left, newRight))
        } else {
          val (opPri, newLeft) = remove(removeKey, left)
          (opPri, Node(key, pri, newLeft, right))
        }
    }
  }

  @scala.annotation.tailrec
  private def getCountByKey(searchKey: K, tree: Tree): Option[Int] = {
    tree match {
      case Leaf => None
      case Node(key: K, pri, left, right) =>
        if (key == searchKey) {
          return Some(pri)
        }
        getCountByKey(searchKey, if (ord.lt(key, searchKey)) right else left)
    }
  }

  private def toString(tree: Tree): String = {
    tree match {
      case Leaf => "[]"
      case Node(key, pri, left, right) =>
        var (leftStr, rightStr) =
          (toString(left).dropRight(1), toString(right).drop(1))
        if (leftStr.length > 1) {
          leftStr += ", "
        }
        if (rightStr.length > 1) {
          rightStr = ", " ++ rightStr
        }
        leftStr + key + " -> " + pri + rightStr
    }
  }

  private def foreach[R](f: (K, Int) => R, tree: Tree): Unit = {
    tree match {
      case Leaf =>
      case Node(key, pri, left, right) =>
        foreach(f, left)
        f(key, pri)
        foreach(f, right)
    }
  }

  private def foreachKey[R, P](f: K => R, tree: Tree): Unit = {
    tree match {
      case Leaf =>
      case Node(key, _, left, right) =>
        foreachKey(f, left)
        f(key)
        foreachKey(f, right)
    }
  }
}
