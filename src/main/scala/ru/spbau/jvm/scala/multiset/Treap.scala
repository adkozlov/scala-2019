package ru.spbau.jvm.scala.multiset

import scala.util.Random

/**
 * TODO
 */
class Treap[T <: Ordered[T]] {

  class Node(
                      val value: T,
                      var priority: Int,
                      var count: Int = 0) {
    var leftChild: Option[Node] = emptyNode()
    var rightChild: Option[Node] = emptyNode()
  }

  private var root: Option[Node] = emptyNode()
  private val rand = new Random(322)

  private def emptyNode(): Option[Node] = {
    return Option.empty[Node]
  }

  private def split(t: Option[Node], v: T): (Option[Node], Option[Node]) = {
    if (t.isEmpty) {
      return (emptyNode(), emptyNode())
    } else {
      val node: Node = t.get
      if (node.value < v) {
        val (l, r) = split(node.rightChild, v)
        node.rightChild = l
        return (t, r)
      } else {
        val (l, r) = split(node.leftChild, v)
        node.leftChild = r
        return (l, t)
      }
    }
  }

  private def merge(l: Option[Node], r: Option[Node]): Option[Node] = {
    if (l.isEmpty) {
      return r;
    }

    if (r.isEmpty) {
      return l;
    }

    if (l.get.priority < r.get.priority) {
      l.get.rightChild = merge(l.get.rightChild, r)
      return l
    } else {
      r.get.leftChild = merge(l, r.get.leftChild)
      return r
    }
  }

  private def lowerBound(t: Option[Node], v: T): Option[Node] = {
    if (t.isEmpty) {
      return emptyNode()
    }

    val node: Node = t.get

    if (node.value < v) {
      return lowerBound(node.rightChild, v)
    } else {
      var result = lowerBound(node.leftChild, v)
      if (result.isEmpty) {
        result = t
      }

      return result
    }
  }

  def get(v: T): Option[Node] = {
    val t = lowerBound(root, v)
    if (t.isDefined && t.get.value == v) {
      return t
    } else {
      return emptyNode()
    }
  }

  def add(v: T): Unit = {
    add(v, 1)
  }

  def add(v: T, count: Int): Unit = {
    val node = get(v)
    if (node.isEmpty) {
      val (l, r) = split(root, v)
      val newNode = Option.apply(new Node(v, rand.nextInt(), count))
      root = merge(merge(l, newNode), r)
    } else {
      node.get.count += count
    }
  }

  def remove(v: T): Boolean = {
    val node = get(v)
    if (node.isEmpty) {
      return false
    }

    node.get.count -= 1
    if (node.get.count == 0) {
      var (l, r) = split(root, v)

      if (r.isDefined) {
        if (r.get.leftChild.isEmpty) {
          r = r.get.rightChild
        } else {
          var current = r
          var previous = emptyNode()
          while (current.get.leftChild.isDefined) {
            previous = current
            current = current.get.leftChild
          }
          previous.get.leftChild = current.get.rightChild
          current.get.rightChild = emptyNode()
        }
      }

      root = merge(l, r)
    }

    return true
  }

  def empty(): Boolean = {
    return root.isEmpty
  }

  def foreachCount[U](f: (T, Int) => U): Unit = {
    foreachCount(root, f)
  }

  def foreach[U](f: T => U): Unit = {
    foreach(root, f)
  }

  def foreachCount[U](v: Option[Node], f: (T, Int) => U): Unit = {
    if (v.isEmpty) {
      return
    }

    foreachCount(v.get.leftChild, f)
    f(v.get.value, v.get.count)
    foreachCount(v.get.rightChild, f)
  }

  //Couldn't figure out how to not copy this :(
  def foreach[U](v: Option[Node], f: T => U): Unit = {
    if (v.isEmpty) {
      return
    }

    foreach(v.get.leftChild, f)
    f(v.get.value)
    foreach(v.get.rightChild, f)
  }

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }
}