package ru.spbau.jvm.scala

import scala.util.Random

class MultiSet[T](initialElements: T*)(implicit val ordering: Ordering[T]) {
  private case class Node(key: T,
                          priority: Double,
                          var left: Node = null,
                          var right: Node = null,
                          var count: Int) {
    override def equals(that: Any): Boolean = that match {
      case Node(k, _, _, _, _) => ordering.equiv(key, k)
      case _                   => false
    }
  }

  private val rand: Random = new Random()

  private var root: Node = _

  initialElements.foreach(add(_))

  @scala.annotation.tailrec
  private def find(node: Node, key: T): Option[Node] = {
    if (node == null) {
      return None
    }
    if (node.key == key) {
      return Some(node)
    }
    if (ordering.lt(node.key, key)) {
      find(node.right, key)
    } else {
      find(node.left, key)
    }
  }

  private def split(node: Node,
                    key: T,
                    keyToRight: Boolean = true): (Node, Node) = {
    if (node == null) {
      return (null, null)
    }

    if ((keyToRight && ordering.lt(node.key, key)) ||
        (!keyToRight && ordering.lteq(node.key, key))) {
      val tmp = split(node.right, key, keyToRight)
      node.right = tmp._1
      (node, tmp._2)
    } else {
      val tmp = split(node.left, key, keyToRight)
      node.left = tmp._2
      (tmp._1, node)
    }
  }

  private def merge(left: Node, right: Node): Node = {
    if (left == null) {
      return right
    }
    if (right == null) {
      return left
    }

    if (left.priority > right.priority) {
      val tmp = merge(left.right, right)
      left.right = tmp
      left
    } else {
      val tmp = merge(left, right.left)
      right.left = tmp
      right
    }
  }

  def add(key: T, count: Int = 1): Unit = {
    if (count < 0) remove(key, -count)
    else {
      find(root, key) match {
        case Some(node) => node.count += count
        case None =>
          val newNode = Node(key, rand.nextDouble(), count = count)
          val tmp = split(root, key)
          root = merge(tmp._1, newNode)
          root = merge(root, tmp._2)
      }
    }
  }

  def apply(key: T): Int = {
    find(root, key) match {
      case Some(node) => node.count
      case None       => 0
    }
  }

  def remove(key: T, count: Int = 1): Unit = {
    if (count < 0) {
      add(key, -count)
    } else {
      find(root, key) match {
        case Some(node) =>
          node.count -= count
          if (node.count <= 0) {
            removeAll(key)
          }
        case None =>
      }
    }
  }

  def removeAll(key: T): Unit = {
    val leftTmp = split(root, key)
    val rightTmp = split(leftTmp._2, key, keyToRight = false)
    root = merge(leftTmp._1, rightTmp._2)
  }

  def |(other: MultiSet[T]): MultiSet[T] = {
    val answer = new MultiSet[T]
    for (key <- this) {
      answer.add(key, this(key))
    }
    for (key <- other) {
      answer.add(key, other(key))
    }
    answer
  }

  def &(other: MultiSet[T]): MultiSet[T] = {
    val answer = new MultiSet[T]
    for (key <- this) {
      val count = math.min(this(key), other(key))
      if (count > 0) {
        answer.add(key, count = count)
      }
    }
    answer
  }

  private def iterateAndApply(node: Node, fun: T => Unit): Unit = {
    if (node == null) {
      return
    }
    iterateAndApply(node.left, fun)
    fun(node.key)
    iterateAndApply(node.right, fun)
  }

  def foreach(fun: T => Unit): Unit = {
    iterateAndApply(root, fun)
  }

  def toList: List[(T, Int)] = {
    val listBuilder = List.newBuilder[(T, Int)]
    for (key <- this) {
      listBuilder.addOne((key, this(key)))
    }
    listBuilder.result()
  }

  def filter(predicate: T => Boolean): MultiSet[T] = {
    val answer = new MultiSet[T]
    for (key <- this) {
      if (predicate(key)) {
        answer.add(key, this(key))
      }
    }
    answer
  }

  def clear(): Unit = {
    root = null
  }

  def withFilter(predicate: T => Boolean): MultiSet[T] = {
    val filtered = filter(predicate)
    clear()
    for (key <- filtered) {
      add(key, filtered(key))
    }
    this
  }

  def map[S](fun: T => S)(implicit newOrdering: Ordering[S]): MultiSet[S] = {
    val result = new MultiSet[S]()(newOrdering)
    for (key <- this) {
      result.add(fun(key), this(key))
    }
    result
  }
}
