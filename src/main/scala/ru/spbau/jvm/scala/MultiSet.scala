package ru.spbau.jvm.scala

import java.util

import scala.util.Random

private case class MultiSetNode[T](key: T, var count: Int, priority: Int, var left: MultiSetNode[T], var right: MultiSetNode[T]) {
  def this(key: T) = {
    this(key, count = 1, Random.nextInt(), null, null)
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

  def insert(key: T): MultiSetNode[T] = {
    if (key.hashCode() < this.key.hashCode()) {
      left = if (left == null) new MultiSetNode[T](key) else left.insert(key)
      if (left.priority > priority) return rotateRight()
    } else if (key.hashCode() > this.key.hashCode()) {
      right = if (right == null) new MultiSetNode[T](key) else right.insert(key)
      if (right.priority > priority) return rotateLeft()
    } else count += 1

    this
  }

  def delete(key: T): MultiSetNode[T] = {
    if (left != null && key.hashCode() < this.key.hashCode()) left = left.delete(key)
    else if (right != null && key.hashCode() > this.key.hashCode()) right = right.delete(key)
    else if (count > 1) count -= 1 else {
      if (left == null) return right
      else if (right == null) return left
      else if (left.priority < right.priority) {
        val tmp = rotateLeft()
        tmp.left = tmp.left.delete(key)
        return tmp
      } else {
        val tmp = rotateRight()
        tmp.right = tmp.right.delete(key)
        return tmp
      }
    }

    this
  }

  def iterator(): Iterator[MultiSetNode[T]] = for {
    t <-
      (if (left != null) left.iterator() else Iterator[MultiSetNode[T]]()) ++
        Iterator[MultiSetNode[T]](this) ++
        (if (right != null) right.iterator() else Iterator[MultiSetNode[T]]())
  } yield t

}

class MultiSet[T](elements: T*) extends util.AbstractCollection[T] {
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
    val it = iterator()
    while (it.hasNext) f(it.next())
  }

  override def size(): Int = _size

  override def add(key: T): Boolean = {
    if (key == null) throw new IllegalArgumentException("key must not be null")

    _root = if (_root == null) new MultiSetNode[T](key) else _root.insert(key)

    _size += 1
    true
  }


  override def remove(key: Any): Boolean = {
    if (key == null) throw new IllegalArgumentException("key must not be null")
    super.remove(key)
  }

  def &(other: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]

    val left = iterator()
    val right = other.iterator()

    var l: Any = null
    var r: Any = null
    while (left.hasNext && right.hasNext) {
      if (l == null) l = left.next()
      if (r == null) r = right.next()

      if (l.hashCode() < r.hashCode()) {
        l = null
      } else if (l.hashCode() > r.hashCode()) {
        r = null
      } else {
        result.add(l.asInstanceOf[T])
        l = null
        r = null
      }
    }

    result
  }

  def |(other: MultiSet[T]): MultiSet[T] = {
    val result = new MultiSet[T]

    val left = other.iterator()
    val right = iterator()

    var l: Any = null
    var r: Any = null
    while (left.hasNext && right.hasNext) {
      if (l == null) l = left.next()
      if (r == null) r = right.next()

      if (l.hashCode() < r.hashCode()) {
        result.add(l.asInstanceOf[T])
        l = null
      } else if (l.hashCode() > r.hashCode()) {
        result.add(r.asInstanceOf[T])
        r = null
      } else {
        result.add(l.asInstanceOf[T])
        l = null
        r = null
      }
    }

    if (l != null) result.add(l.asInstanceOf[T])
    if (r != null) result.add(r.asInstanceOf[T])

    while (left.hasNext) {
      result.add(left.next())
    }

    while (right.hasNext) {
      result.add(right.next())
    }

    result
  }

  override def toString: String = if (_root != null) _root.iterator().map(node => s"${node.key} -> ${node.count}").mkString("[", ", ", "]") else "[]"

  override def iterator(): util.Iterator[T] = new util.Iterator[T] {
    private val iterator = if (_root == null) Iterator[MultiSetNode[T]]() else _root.iterator()

    private var current: MultiSetNode[T] = _
    private var count = 0
    private var removed = false

    override def hasNext: Boolean = (current != null && count < current.count) || iterator.hasNext

    override def next(): T = {
      removed = false

      if (current != null) {
        if (count < current.count) {
          count += 1
          return current.key
        }
      }

      current = iterator.next()
      count = 1
      current.key
    }

    override def remove(): Unit = {
      if (current != null) {
        if (!removed) {
          _root = _root.delete(current.key)
          _size -= 1
          removed = true
        } else throw new IllegalStateException("the remove method has already been called")
      } else throw new IllegalStateException("the next method has not yet been called")
    }
  }
}
