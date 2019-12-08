package ru.spbau.jvm.scala

sealed trait Tree[K, V] {
  def key: K = throw new UnsupportedOperationException
  def value: V = throw new UnsupportedOperationException
  def priority: Int = throw new UnsupportedOperationException
  def size: Int = throw new UnsupportedOperationException

  def leftChild: Tree[K, V] = throw new UnsupportedOperationException
  def rightChild: Tree[K, V] = throw new UnsupportedOperationException

  def split(splitKey: K)(implicit ord: Ordering[K]): (Tree[K, V], Tree[K, V]) = {
    this match {
      case Node(key, value, priority, _, left, right) =>
        if (ord.lt(splitKey, key)) {
          val (lNew, rNew) = left.split(splitKey)
          (lNew, Node(key, value, priority, getSize(rNew, right), rNew, right))
        } else {
          val (lNew, rNew) = right.split(splitKey)
          (Node(key, value, priority, getSize(left, lNew), left, lNew), rNew)
        }
      case Leaf() => (Leaf(), Leaf())
    }
  }

  def insert(node: Node[K, V])(implicit ord: Ordering[K]): Tree[K, V] = {
    this match {
      case Node(key, value, priority, _, left, right) =>
        if (priority < node.priority) {
          val (lNew, rNew) = split(node.key)
          Node(node.key, node.value, node.priority, getSize(lNew, rNew), lNew, rNew)
        } else {
          val (lNew, rNew) = if (ord.lt(node.key, key)) (left.insert(node), right) else (left, right.insert(node))
          Node(key, value, priority, getSize(lNew, rNew), lNew, rNew)
        }
      case Leaf() => node
    }
  }

  def merge(tree: Tree[K, V])(implicit ord: Ordering[K]): Tree[K, V] = {
    (this, tree) match {
      case (Leaf(), _) => tree
      case (_, Leaf()) => this
      case _ =>
        if (priority > tree.priority) {
          val (lNew, rNew) = (leftChild, rightChild.merge(tree))
          Node(key, value, priority, getSize(lNew, rNew), lNew, rNew)
        } else {
          val (lNew, rNew) = (merge(tree.leftChild), tree.rightChild)
          Node(tree.key, tree.value, tree.priority, getSize(lNew, rNew), lNew, rNew)
        }
    }
  }

  def erase(searchKey: K)(implicit ord: Ordering[K]): Tree[K, V] = {
    this match {
      case Leaf() => Leaf()
      case Node(_, _, _, _, _, _) =>
        if (key == searchKey) {
          return leftChild.merge(rightChild)
        }

        val (lNew, rNew) = if (ord.lt(searchKey, key)) (leftChild.erase(searchKey), rightChild) else
          (leftChild, rightChild.erase(searchKey))
        Node(key, value, priority, getSize(lNew, rNew), lNew, rNew)
    }
  }

  def find(searchKey: K)(implicit ord: Ordering[K]): Option[Tree[K, V]] = {
    this match {
      case Leaf() => None
      case Node(key, _, _, _, left, right) =>
        if (key == searchKey) return Some(this)

        if (ord.lt(searchKey, key)) left.find(searchKey) else right.find(searchKey)
    }
  }

  def toList(implicit ord: Ordering[K]): List[(K, V)] = {
    this match {
      case Node(key, value, _, _, leftChild, rightChild) =>
        leftChild.toList ++ List((key, value)) ++ rightChild.toList
      case Leaf() => List.empty
    }
  }

  def getSize(left: Tree[K, V], right: Tree[K, V]): Int = {
    left.size + right.size + 1
  }
}

case class Node[K, V](override val key: K, override val value: V, override val priority: Int, override val size: Int,
                        override val leftChild: Tree[K, V], override val rightChild: Tree[K, V])
                     (implicit ord: Ordering[K]) extends Tree[K, V]

case class Leaf[K, V]() extends Tree[K, V]() {
  override def size = 0
}