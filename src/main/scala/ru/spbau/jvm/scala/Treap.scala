package ru.spbau.jvm.scala

object Treap {
  def split[K, P](key: K, node: Treap[K, P])(
    implicit ord: K => Ordered[K]
  ): (Treap[K, P], Treap[K, P]) = {
    if (node == null) {
      return (null, null)
    }

    if (node.key < key) {
      val (t1, t2) = split(key, node.rightVal)
      node.setRight(t1)
      (node, t2)
    } else {
      val (t1, t2) = split(key, node.leftVal)
      node.setLeft(t2)
      (t1, node)
    }
  }

  def merge[K, P](t1: Treap[K, P], t2: Treap[K, P])(
    implicit ord: P => Ordered[P]
  ): Treap[K, P] = {
    if (t1 == null) {
      return t2
    }

    if (t2 == null) {
      return t1
    }

    if (t1.priVal < t2.priVal) {
      val t = merge(t1, t2.leftVal)
      t2.setLeft(t)
      t2
    } else {
      val t = merge(t1.rightVal, t2)
      t1.setRight(t)
      t1
    }
  }
}

class Treap[K, P](
  private var key: K,
  private var pri: P,
  private var left: Treap[K, P] = null,
  private var right: Treap[K, P] = null
)(implicit ordK: K => Ordered[K], ordP: P => Ordered[P]) {

  def keyVal: K = key

  def priVal: P = pri

  def leftVal: Treap[K, P] = left

  def rightVal: Treap[K, P] = right

  def setLeft(node: Treap[K, P]): Unit = left = node

  def setRight(node: Treap[K, P]): Unit = right = node

  def insert(k: K, p: P): Treap[K, P] = {
    val v = new Treap(k, p)
    var (t1, t2) = Treap.split(k, this)
    t1 = Treap.merge(t1, v)
    Treap.merge(t1, t2)
  }

  def remove(k: K): (Option[P], Treap[K, P]) = {
    if (key == k) {
      return (Some(pri), Treap.merge(left, right))
    }

    if (key < k) {
      if (right != null) {
        val (p, newRight) = right.remove(k)
        return (p, new Treap(key, pri, left, newRight))
      }
    } else {
      if (left != null) {
        val (p, newLeft) = left.remove(k)
        return (p, new Treap(key, pri, newLeft, right))
      }
    }

    (None, this)
  }

  def toList: List[(K, P)] = {
    val listElem = List((key, pri))

    (left, right) match {
      case (null, null) => listElem
      case (null, _)    => listElem ++ right.toList
      case (_, null)    => left.toList ++ listElem
      case (_, _)       => left.toList ++ listElem ++ right.toList
    }
  }

  def iterator(): Iterator[(K, P)] = toList.iterator

  def keyIterator(): Iterator[K] = {
    iterator().map(x => x._1)
  }

  def getPri(k: K): Option[P] = {
    if (key == k) {
      return Some(pri)
    }

    if (key < k) {
      if (right != null) {
        return right.getPri(k)
      }
    } else {
      if (left != null) {
        return left.getPri(k)
      }
    }

    None
  }
}
