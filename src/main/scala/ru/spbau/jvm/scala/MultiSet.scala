package ru.spbau.jvm.scala

import java.util.StringJoiner

import scala.util.Random

class MultiSet(private val init: Int*) {
  private val generator = new Random(42)

  private class Node(var left: Option[Node], var right: Option[Node], val value: Int,
                     var count: Int) {
    val y: Int = generator.nextInt()
  }

  private var treeSize = 0
  private var tree = Option.empty[Node]
  fillTree(init.toList)


  def &(that: MultiSet): MultiSet = {
    val result = new MultiSet()
    pairForeach(tree, (value: Int, count: Int) => {
      val minCount = Math.min(count, that.getCount(value))
      if (minCount > 0)
        result.insert(value, minCount)
    })
    result
  }

  def |(that: MultiSet): MultiSet = {
    val result = new MultiSet()
    foreach(value => result.add(value))
    that.foreach(value => result.add(value))
    result
  }

  def map(f: Int => Int): MultiSet = {
    val newMultiSet = new MultiSet()
    this.foreach(value => newMultiSet.add(f(value)))
    newMultiSet
  }

  def foreach(f: Int => Unit): Unit = {
    foreach(tree, f)
  }

  def size(): Int = {
    treeSize
  }

  def contains(value: Int): Boolean = {
    get(value).nonEmpty
  }

  def getCount(value: Int): Int = {
    val node = get(value)
    if (node.isEmpty) 0 else node.get.count
  }

  def add(value: Int): Unit = {
    val getValue = get(value)
    if (getValue.isEmpty) {
      insert(value, 1)
    } else {
      getValue.get.count += 1
    }
  }

  def remove(value: Int): Unit = {
    val getValue = get(value)
    if (getValue.nonEmpty) {
      if (getValue.get.count == 1) {
        erase(value)
      } else {
        getValue.get.count -= 1
      }
    }
  }

  override def toString(): String = {
    val stringJoiner = new StringJoiner(", ", "[", "]")
    pairForeach(tree, (value: Int, count: Int) => stringJoiner.add(value + " -> " + count))
    stringJoiner.toString
  }

  private def foreach(node: Option[Node], f: Int => Unit): Unit = {
    node.foreach(node => {
      foreach(node.left, f)
      for (_ <- 1 to node.count) {
        f(node.value)
      }
      foreach(node.right, f)
    })
  }

  private def pairForeach(node: Option[Node], f: (Int, Int) => Unit): Unit = {
    node.foreach(node => {
      pairForeach(node.left, f)
      f(node.value, node.count)
      pairForeach(node.right, f)
    })
  }

  private def fillTree(init: List[Int]): Unit = {
    for (x <- init) {
      add(x)
    }
  }

  private def insert(value: Int, count: Int): Unit = {
    val (l, r) = split(tree, value)
    val newNode = Option.apply(new Node(Option.empty[Node], Option.empty[Node], value, count))
    tree = merge(merge(l, newNode), r)
    treeSize += count
  }

  private def erase(value: Int): Unit = {
    val count = getCount(value)
    val (l, m) = split(tree, value)
    val (_, r) = split(m, value + 1)
    tree = merge(l, r)
    treeSize -= count
  }

  private def get(value: Int): Option[Node] = {
    val node = lowerBound(tree, value)
    if (node.isEmpty || node.get.value != value) {
      Option.empty[Node]
    } else {
      node
    }
  }

  private def lowerBound(t: Option[Node], value: Int): Option[Node] = {
    if (t.isEmpty) {
      return Option.empty[Node]
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
      if (l.isEmpty) {
        return r
      } else {
        return l
      }
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

  private def split(t: Option[Node], k: Int): (Option[Node], Option[Node]) = {
    if (t.isEmpty) {
      return (Option.empty[Node], Option.empty[Node])
    }
    if (t.get.value < k) {
      val (l, r) = split(t.get.right, k)
      t.get.right = l
      (t, r)
    }
    else {
      val (l, r) = split(t.get.left, k);
      t.get.left = r
      (l, t)
    }
  }
}