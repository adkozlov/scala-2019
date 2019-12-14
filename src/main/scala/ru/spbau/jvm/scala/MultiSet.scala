package ru.spbau.jvm.scala

import scala.util.Random

class MultiSet[T](elements: T*)(implicit ord: Ordering[T]){
  private case class Node
    (key: T,
     var count: Int,
     priority: Int,
     var leftChild: Node = null,
     var rightChild: Node = null,
     var parent: Node = null)

  private val rand: Random = new Random()

  private var root: Node = _

  elements.foreach(add(_))

  def add(key: T, delta: Int = 1)
         (implicit ord: Ordering[T]): Unit = {
    if (delta <= 0) {
      return
    }
    val found = findNode(root, key)
    if (found.isEmpty) {
      val newNode = Node(key, delta, rand.nextInt())
      val splitted = split(root, key)
      root = merge(merge(splitted._1, newNode), splitted._2)
    } else {
      found.get.count += delta
    }
  }

  def apply(key: T)
           (implicit ord: Ordering[T]): Int = {
    findNode(root, key) match {
      case Some(node) => node.count
      case _ => 0
    }
  }

  def remove(key: T, delta: Int = 1)
            (implicit ord: Ordering[T]): Unit = {
    val found = findNode(root, key)
    if (found.isEmpty) {
      return
    }
    found.get.count -= delta
    if (found.get.count <= 0) {
      removeAll(key)
    }
  }

  def removeAll(key: T)
               (implicit ord: Ordering[T]): Unit = {
    val found = findNode(root, key)
    if (found.isEmpty) {
      return
    }
    val next = findNext(found.get)
    val splitted = split(root, key)
    if (next.isEmpty) {
      root = splitted._1
    } else {
      val splittedRight = split(splitted._2, next.get.key)
      root = merge(splitted._1, splittedRight._2)
    }
  }

  def |(other: MultiSet[T])
       (implicit ord: Ordering[T]): MultiSet[T] = {
    val result = new MultiSet[T]
    for (key <- this) {
      result.add(key, this(key))
    }
    for (key <- other) {
      result.add(key, other(key))
    }
    result
  }

  def &(other: MultiSet[T])
       (implicit ord: Ordering[T]): MultiSet[T] = {
    val result = new MultiSet[T]
    for (key <- this) {
      val otherCount = other(key)
      if (otherCount > 0) {
        result.add(key, math.min(this(key), otherCount))
      }
    }
    result
  }

  def foreach(fun: T => Unit)
             (implicit ord: Ordering[T]): Unit = {
    foreach(root, fun)
  }

  def toList(implicit ord: Ordering[T]): List[(T, Int)] = {
    val builder = List.newBuilder[(T, Int)]
    for (key <- this) {
        builder += ((key, this(key)))
    }
    builder.result()
  }

  def filter(predicate: T => Boolean)
            (implicit ord: Ordering[T]): MultiSet[T] = {
    val result = new MultiSet[T]
    for (key <- this) {
      if (predicate(key)) {
        result.add(key, this(key))
      }
    }
    result
  }

  def map[S](fun: T => S) (implicit ord: Ordering[T],
                           ord2: Ordering[S]): MultiSet[S] = {
    val result = new MultiSet[S]
    for (key <- this) {
      result.add(fun(key), this(key))
    }
    result
  }

  def clear(): Unit = {
    root = null
  }

  private def findNext(node: Node): Option[Node] = {
    if (node.rightChild != null) {
      var targetNode = node.rightChild
      while (targetNode.leftChild != null) {
        targetNode = targetNode.leftChild
      }
      return Option(targetNode)
    }
    var targetNode = node
    while (targetNode.parent != null && targetNode ==
      targetNode.parent.rightChild) {
      targetNode = targetNode.parent
    }
    if (targetNode.parent != null) {
      return Option(targetNode.parent)
    }
    Option.empty
  }

  private def foreach(node: Node, fun: T => Unit)
                     (implicit ord: Ordering[T]): Unit = {
    if (node == null) {
      return
    }
    foreach(node.leftChild, fun)
    fun(node.key)
    foreach(node.rightChild, fun)
  }

  private def findNode(node: Node, key: T)
                      (implicit ord: Ordering[T]): Option[Node] = {
    if (node == null) {
      return Option.empty
    }
    if (node.key == key) {
      return Option(node)
    }
    if (ord.lt(node.key, key)) {
      return findNode(node.rightChild, key)
    }
    findNode(node.leftChild, key)
  }

  private def merge(left: Node, right: Node): Node = {
    if (left == null) {
      return right
    }
    if (right == null) {
      return left
    }

    if (left.priority > right.priority) {
      val tmp = merge(left.rightChild, right)
      left.rightChild = tmp
      if (tmp != null) {
        tmp.parent = left
      }
      return left
    }

    val tmp = merge(left, right.leftChild)
    right.leftChild = tmp
    if (tmp != null) {
      tmp.parent = right
    }
    right
  }

  private def split(node: Node, key: T)
                   (implicit ord: Ordering[T]): (Node, Node) = {
    if (node == null) {
      return (null, null)
    }

    if (ord.lt(node.key, key)) {
      val splitted = split(node.rightChild, key)
      node.rightChild = splitted._1
      if (splitted._1 != null) {
        splitted._1.parent = node
      }
      return (node, splitted._2)
    }

    val splitted = split(node.leftChild, key)
    node.leftChild = splitted._2
    if (splitted._2 != null) {
      splitted._2.parent = node
    }
    (splitted._1, node)
  }
}
