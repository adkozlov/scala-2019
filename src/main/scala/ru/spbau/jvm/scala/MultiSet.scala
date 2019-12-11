package ru.spbau.jvm.scala

import scala.util.Random

class MultiSet[V](private val count: Int = 0, private val tree: Tree[V, Pair[V, Int]] = Leaf[V, Pair[V, Int]]())(implicit ord: Ordering[V]) {
  def size(): Int = count

  def isEmpty: Boolean = count == 0

  def contains(value: V): Boolean = tree.find(value).isDefined

  def add(value: V): MultiSet[V] = {
    tree.find(value) match {
      case Some(node) =>
        MultiSet(count + 1, tree.erase(value).insert(mutateNode(node, Pair(node.key, node.value.value + 1))))
      case None => MultiSet(count + 1, tree.insert(Node(value, Pair(value, 1), Random.nextInt(), 1, Leaf(), Leaf())))
    }
  }

  def remove(value: V): MultiSet[V] = {
    tree.find(value) match {
      case Some(node) =>
        if (node.value.value == 1) MultiSet[V](count - 1, tree.erase(value))
        else
          MultiSet[V](count - 1, tree.erase(value).insert(mutateNode(node, Pair(node.key, node.value.value - 1))))
      case None => this
    }
  }

  private def mutateNode(node : Tree[V, Pair[V, Int]], newVal : Pair[V, Int]): Node[V, Pair[V, Int]] = {
    Node(node.key, newVal, node.priority, node.size, node.leftChild, node.rightChild)
  }

  def clear(): MultiSet[V] = {
    MultiSet()
  }

  private def forCopy[U](f : V => U)(pair : Pair[V, Int]): Unit = {
    var numberOfCopies = pair.value
    while (numberOfCopies > 0) {
      f(pair.key)
      numberOfCopies -= 1
    }
  }

  def foreach[U](f: V => U): Unit = {
    tree.foreach(forCopy(f))
  }

  def filter(p: V => Boolean): MultiSet[V] = {
    var multiSet = MultiSet()

    for (node <- tree) {
      if (p(node.key)) {
        forCopy(x => multiSet = multiSet.add(x))(node)
      }
    }

    multiSet
  }

  def withFilter(p: V => Boolean): MultiSet[V] = {
    filter(p)
  }

  def map[U](f : V => U)(implicit ord: Ordering[U]): MultiSet[U] = {
    var multiSet = MultiSet()

    for (node <- tree) {
      forCopy(x => multiSet = multiSet.add(f(x)))(node)
    }

    multiSet
  }

  def flatMap[U](f : V => MultiSet[U])(implicit ord: Ordering[U]): MultiSet[U] = {
    var multiSet = MultiSet()

    for (node <- tree) {
      forCopy(x => multiSet = multiSet | f(x))(node)
    }

    multiSet
  }

  def |(other: MultiSet[V]): MultiSet[V] = {
    var multiSet = MultiSet()

    for (node <- tree) {
      forCopy(x => multiSet = multiSet.add(x))(node)
    }

    for (node <- other.tree) {
      forCopy(x => multiSet = multiSet.add(x))(node)
    }

    multiSet
  }

  def &(other: MultiSet[V]): MultiSet[V] = {
    var multiSet = MultiSet()

    for (node1 <- tree; node2 <- other.tree if node2.key == node1.key) {
      forCopy(x => multiSet = multiSet.add(x))(
        Pair(node1.key, if (node1.value < node2.value) node1.value else node2.value))
    }

    multiSet
  }

  def apply(value: V): Int = {
    tree.find(value) match {
      case Some(node) => node.value.value
      case _          => 0
    }
  }

  override def toString: String = {
    var string = "["

    for (node <- tree) {
      string += node.key + " -> " + node.value + ", "
    }

    (if (string.length == 1) string else string.reverse.drop(2).reverse) + "]"
  }
}

object MultiSet {
  def apply[V]()(implicit ord: Ordering[V]): MultiSet[V] = MultiSet(0, Leaf[V, Pair[V, Int]]())

  def apply[V](count: Int, tree: Tree[V, Pair[V, Int]])(implicit ord: Ordering[V]): MultiSet[V] = new MultiSet(count, tree)

  // It seems that the only way to create constructor that accepts variable amount of parameters is
  // to accept V* in it.
  // But V* turns out to be syntactic sugar for IterableOnce[V], which is part of the collection package.
  // So I don't know, whether it is forbidden or not to create this constructor :C
  // Although, judging from the task, there should exist such a constructor!
  def apply[V](values: V*)(implicit ord: Ordering[V]): MultiSet[V] = values.foldLeft(MultiSet[V]())((set, value) => set.add(value))
}
