package ru.spbau.jvm.scala

import java.util.StringJoiner

import scala.util.Random

/**
 * Implementation of multiset based on treap
 *
 * @param init elements to build treap with
 */
class MultiSet[T <: Ordered[T]](private val init: T*) {
  private val generator = new Random(42)

  private class Node(var left: Option[Node], var right: Option[Node], val value: T,
                     var count: Int) {
    val y: Int = generator.nextInt()
  }

  private var treeSize = 0
  private var tree = Option.empty[Node]
  for (x <- init) {
    add(x)
  }

  /** Returns a new multiset that is an intersection of 2 given multisets */
  def &(that: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]()
    pairForeach(tree, (value: T, count: Int) => {
      val minCount = Math.min(count, that.getCount(value))
      if (minCount > 0)
        result.insert(value, minCount)
    })
    result
  }

  /** Returns a new multiset that is a union of 2 given multisets */
  def |(that: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]()
    foreach(value => result.add(value))
    that.foreach(value => result.add(value))
    result
  }

  /** Returns a new multiset constructed from the all values of the given multiset with the function applied */
  def map[V <: Ordered[V]](f: T => V): MultiSet[V] = {
    val newMultiSet = new MultiSet[V]()
    this.foreach(value => newMultiSet.add(f(value)))
    newMultiSet
  }

  /** Applies function to every value in the multiset */
  def foreach(f: T => Unit): Unit = {
    foreach(tree, f)
  }

  /** Returns size of the multiset */
  def size(): Int = {
    treeSize
  }

  /** Returns if the value contains in the multiset */
  def contains(value: T): Boolean = {
    get(value).nonEmpty
  }

  /** Returns how many times the value occurs in the multiset */
  def getCount(value: T): Int = {
    get(value) match {
      case Some(node) => node.count
      case _ => 0
    }
  }

  /** Adds one occurrence of the value to the multiset */
  def add(value: T): Unit = {
    get(value) match {
      case None => insert(value, 1)
      case Some(node) => node.count += 1
    }
  }

  /** Removes one occurrence of the value from the multiset */
  def remove(value: T): Unit = {
    get(value).foreach(node => {
      node.count match {
        case 1 => erase(value)
        case _ => node.count -= 1
      }
    })
  }

  override def toString: String = {
    val stringJoiner = new StringJoiner(", ", "[", "]")
    pairForeach(tree, (value: T, count: Int) => stringJoiner.add(value + " -> " + count))
    stringJoiner.toString
  }

  private def foreach(node: Option[Node], f: T => Unit): Unit = {
    node.foreach(node => {
      foreach(node.left, f)
      for (_ <- 1 to node.count) {
        f(node.value)
      }
      foreach(node.right, f)
    })
  }

  private def pairForeach(node: Option[Node], f: (T, Int) => Unit): Unit = {
    node.foreach(node => {
      pairForeach(node.left, f)
      f(node.value, node.count)
      pairForeach(node.right, f)
    })
  }

  private def insert(value: T, count: Int): Unit = {
    val (l, r) = split(tree, value)
    val newNode = Option.apply(new Node(None, None, value, count))
    tree = merge(merge(l, newNode), r)
    treeSize += count
  }

  private def erase(value: T): Unit = {
    val count = getCount(value)
    val (l, m) = split(tree, value)
    val r = removeSmallestKey(m)
    tree = merge(l, r)
    treeSize -= count
  }

  private def removeSmallestKey(t: Option[Node]): Option[Node] = {
    if (t.isEmpty) {
      return t
    }

    if (t.get.left.isEmpty) {
      val tmp = t.get.right
      t.get.right = None
      return tmp
    }

    var node = t
    var parent = Option.empty[Node]
    while (node.get.left.nonEmpty) {
      parent = node
      node = node.get.left
    }
    parent.get.left = node.get.right
    node.get.right = None
    t
  }

  private def get(value: T): Option[Node] = {
    val node = lowerBound(tree, value)
    if (node.isEmpty || node.get.value != value) {
      None
    } else {
      node
    }
  }

  private def lowerBound(t: Option[Node], value: T): Option[Node] = {
    if (t.isEmpty) {
      return None
    }
    if (t.get.value < value) {
      lowerBound(t.get.right, value)
    } else {
      val tmp = lowerBound(t.get.left, value)
      if (tmp.isEmpty) {
        t
      } else {
        tmp
      }
    }
  }

  private def merge(l: Option[Node], r: Option[Node]): Option[Node] = {
    if (l.isEmpty || r.isEmpty) {
      return if (l.isEmpty) r else l
    }

    if (l.get.y < r.get.y) {
      val tmp = merge(l.get.right, r)
      l.get.right = tmp
      l
    }
    else {
      val tmp = merge(l, r.get.left)
      r.get.left = tmp
      r
    }
  }

  private def split(t: Option[Node], k: T): (Option[Node], Option[Node]) = {
    if (t.isEmpty) {
      return (None, None)
    }
    if (t.get.value < k) {
      val (l, r) = split(t.get.right, k)
      t.get.right = l
      (t, r)
    }
    else {
      val (l, r) = split(t.get.left, k)
      t.get.left = r
      (l, t)
    }
  }
}