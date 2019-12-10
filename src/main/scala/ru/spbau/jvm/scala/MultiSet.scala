package ru.spbau.jvm.scala

import scala.math.{max, min}
import scala.util.Random

private case class MultiSetNode[T](key: T, var count: Int, priority: Int, var left: MultiSetNode[T], var right: MultiSetNode[T]) {
  def this(key: T, c: Int = 1) = {
    this(key, c, Random.nextInt(), null, null)
  }

  private def rotateRight(): MultiSetNode[T] = {
    val x = this.left
    val y = x.right

    x.right = this
    this.left = y

    x
  }

  private def rotateLeft(): MultiSetNode[T] = {
    val y = this.right
    val x = y.left

    y.left = this
    this.right = x

    y
  }

  def find(key: T): MultiSetNode[T] = {
    if (this.key == key) return this

    if (this.key.hashCode() < key.hashCode()) {
      if (right != null) right.find(key) else null
    } else {
      if (left != null) left.find(key) else null
    }
  }

  def insert(key: T, c: Int = 1): MultiSetNode[T] = {
    if (key.hashCode() < this.key.hashCode()) {
      left = if (left == null) new MultiSetNode[T](key, c) else left.insert(key)
      if (left.priority > priority) return rotateRight()
    } else if (key.hashCode() > this.key.hashCode()) {
      right = if (right == null) new MultiSetNode[T](key, c) else right.insert(key)
      if (right.priority > priority) return rotateLeft()
    } else count += c

    this
  }

  def delete(key: T, callback: T => Unit): MultiSetNode[T] = {
    if (left != null && key.hashCode() < this.key.hashCode()) left = left.delete(key, callback)
    else if (right != null && key.hashCode() > this.key.hashCode()) right = right.delete(key, callback)
    else if (key.hashCode() == this.key.hashCode()) {
      callback(this.key)
      if (count > 1) count -= 1 else {
        if (left == null) return right
        else if (right == null) return left
        else if (left.priority < right.priority) {
          val tmp = rotateLeft()
          tmp.left = tmp.left.delete(key, callback)
          return tmp
        } else {
          val tmp = rotateRight()
          tmp.right = tmp.right.delete(key, callback)
          return tmp
        }
      }
    }

    this
  }

  def toList(): List[T] = {
    var list: List[T] = Nil
    for (_ <- 1 to this.count) {
      list = key :: list
    }

    (if (left != null) left.toList() else Nil) ::: list ::: (if (right != null) right.toList() else Nil)
  }

  def toNodeList(): List[MultiSetNode[T]] = {
    (if (left != null) left.toNodeList() else Nil) ::: this :: (if (right != null) right.toNodeList() else Nil)
  }
}


class MultiSet[T](elements: T*) {
  private var _size = 0
  private var _root: MultiSetNode[T] = _

  elements.foreach {e => add(e)}

  def map[U](f: T => U): MultiSet[U] = {
    val result = new MultiSet[U]
    this.foreach { element => result.add(f(element)) }
    result
  }

  def withFilter(f: T => Boolean): MultiSet[T] = {
    val result = new MultiSet[T]
    this.foreach { element => if (f(element)) result.add(element) }
    result
  }

  def foreach[U](f: T => U): Unit = {
    for (value <- _root.toList()) f(value)
  }

  def size(): Int = _size

  def add(key: T, count: Int = 1): MultiSet[T] = {
    if (key == null) throw new IllegalArgumentException("key must not be null")

    _root = if (_root == null) new MultiSetNode[T](key, count) else _root.insert(key, count)

    _size += count
    this
  }

  def remove(key: T): MultiSet[T] = {
    if (key == null) throw new IllegalArgumentException("key must not be null")

    if (_root != null) {
      _root = _root.delete(key, { _ => _size -= 1 })
    }

    this
  }

  def count(key: T): Int = {
    if (key == null) throw new IllegalArgumentException("key must not be null")

    if (_root != null) {
      val node = _root.find(key)
      if (node == null) 0 else node.count
    } else 0
  }

  def contains(key: T): Boolean = {
    if (key == null) throw new IllegalArgumentException("key must not be null")

    if (_root != null) {
      val node = _root.find(key)
      node != null
    } else false
  }

  def &(other: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]

    for {
      left <- _root.toNodeList()
      right <- other._root.toNodeList()
      if left.key == right.key
    } {
      val size = min(left.count, right.count)
      result.add(left.key, size)
    }

    result
  }

  def |(other: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]

    for {
      left <- _root.toNodeList()
      right <- other._root.toNodeList()
      if left.key == right.key
    } {
      val size = max(left.count, right.count)
      result.add(left.key, size)
    }

    def insertIfNotContains(node: MultiSetNode[T]): Unit = {
      if (!result.contains(node.key)) {
        result.add(node.key, node.count)
      }
    }

    for { node <- _root.toNodeList() } insertIfNotContains(node)
    for { node <- other._root.toNodeList() } insertIfNotContains(node)

    result
  }

  override def toString: String = {
    if (_root == null) return "[]"

    val result = new StringBuilder
    result.append("[")
    _root.toNodeList().foreach(node => result.append(s"${node.key} -> ${node.count}, "))
    result.setLength(result.length() - 2)
    result.append("]")

    result.toString()
  }

}
