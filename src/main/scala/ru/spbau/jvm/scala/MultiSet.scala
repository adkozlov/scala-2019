package ru.spbau.jvm.scala

import scala.Ordering.Implicits._
import scala.math.{max, min}
import scala.util.Random

/*
 * Cartesian tree
 */
private sealed trait Tree[+T, +V] {
  def key: T

  def priority: Int

  def value: V

  def left: Tree[T, V]

  def right: Tree[T, V]
}

private final case class Node[+T, +V](key: T, priority: Int, value: V, left: Tree[T, V], right: Tree[T, V]) extends Tree[T, V]

private case object Leaf extends Tree[Nothing, Nothing] {
  override def key: Nothing = throw new NoSuchElementException

  override def priority: Int = throw new NoSuchElementException

  override def value: Nothing = throw new NoSuchElementException

  override def left: Tree[Nothing, Nothing] = throw new NoSuchElementException

  override def right: Tree[Nothing, Nothing] = throw new NoSuchElementException
}

private object Tree {
  def split[T: Ordering, V](tree: Tree[T, V], k: T): (Tree[T, V], Tree[T, V]) = {
    tree match {
      case Leaf => (Leaf, Leaf)
      case Node(key, priority, value, left, right) => key match {
        case x if x < k =>
          val (l, r) = split(right, k)
          (Node(key, priority, value, left, l), r)
        case _ =>
          val (l, r) = split(left, k)
          (l, Node(key, priority, value, r, right))
      }
    }
  }

  def merge[T: Ordering, V](l: Tree[T, V], r: Tree[T, V]): Tree[T, V] = {
    (l, r) match {
      case (Leaf, r) => r
      case (l, Leaf) => l
      case x if x._1.priority < x._2.priority => Node(r.key, r.priority, r.value, merge(l, r.left), r.right)
      case _ => Node(l.key, l.priority, l.value, l.left, merge(l.right, r))
    }
  }

  def updateIfExist[T: Ordering, V](tree: Tree[T, V], k: T, f: Node[T, V] => Tree[T, V]): Tree[T, V] = {
    tree match {
      case Leaf => Leaf
      case t@Node(key, priority, value, left, right) => key match {
        case x if x < k => Node(key, priority, value, left, updateIfExist(right, k, f))
        case x if x > k => Node(key, priority, value, updateIfExist(left, k, f), right)
        case _ => f(t)
      }
    }
  }

  @scala.annotation.tailrec
  def find[T: Ordering, V](tree: Tree[T, V], k: T): Tree[T, V] = {
    tree match {
      case Leaf => Leaf
      case t@Node(key, _, _, left, right) => key match {
        case x if x < k => find(right, k)
        case x if x > k => find(left, k)
        case _ => t
      }
    }
  }

  def insert[T: Ordering, V](tree: Tree[T, V], k: T, p: Int, v: V): Tree[T, V] = {
    val (l, r) = split(tree, k)
    merge(merge(l, Node(k, p, v, Leaf, Leaf)), r)
  }

  def remove[T: Ordering, V](tree: Tree[T, V], k: T): Tree[T, V] =
    updateIfExist[T, V](tree, k, node => merge(node.left, node.right))

  def foreach[T: Ordering, V, U](tree: Tree[T, V], f: (T, V) => U): Unit =
    tree match {
      case Leaf =>
      case Node(key, _, value, left, right) =>
        foreach(left, f)
        f(key, value)
        foreach(right, f)
    }

}

case class MultiSet[T: Ordering](elements: T*) {
  private var _tree: Tree[T, Int] = Leaf
  private var _size: Int = 0
  private val _rand: Random = scala.util.Random

  elements.foreach { e => add(e) }

  def size(): Int = _size

  def add(element: T): Boolean = {
    Tree.find(_tree, element) match {
      case Leaf =>
        _tree = Tree.insert(_tree, element, _rand.nextInt(), 1)
      case _ => _tree = Tree.updateIfExist[T, Int](_tree, element, n => Node(n.key, n.priority, n.value + 1, n.left, n.right))
    }
    _size = _size + 1
    true
  }

  def remove(element: T): Boolean = {
    var deleted = false
    _tree = Tree.updateIfExist[T, Int](_tree, element, {
      node =>
        deleted = true
        _size = _size - 1
        node.value match {
          case 1 => Tree.merge(node.left, node.right)
          case _ => Node(node.key, node.priority, node.value - 1, node.left, node.right)
        }
    })
    deleted
  }

  def contains(element: T): Boolean = {
    Tree.find(_tree, element) match {
      case Leaf => false
      case _ => true
    }
  }

  override def toString: String = {
    var string = "["
    Tree.foreach(_tree, { (key: T, count: Int) => string += s"$key -> $count, " })
    string.length match {
      case 1 =>
      case _ => string = string.dropRight(2)
    }
    string + "]"
  }

  def |(other: MultiSet[T]): MultiSet[T] = {
    var resultTree: Tree[T, Int] = Leaf
    var resultSize: Int = 0

    val functor: (T, Int) => Unit = {
      (key: T, _: Int) =>
        Tree.find(resultTree, key) match {
          case Node(_, _, _, _, _) =>
          case Leaf => (Tree.find(this._tree, key), Tree.find(other._tree, key)) match {
            case (Node(_, _, value, _, _), Leaf) =>
              resultSize = resultSize + value
              resultTree = Tree.insert(resultTree, key, _rand.nextInt(), value)
            case (Leaf, Node(_, _, value, _, _)) =>
              resultSize = resultSize + value
              resultTree = Tree.insert(resultTree, key, _rand.nextInt(), value)
            case (Node(_, _, value1, _, _), Node(_, _, value2, _, _)) =>
              resultSize = resultSize + max(value1, value2)
              resultTree = Tree.insert(resultTree, key, _rand.nextInt(), max(value1, value2))
            case (Leaf, Leaf) =>
          }
        }
    }

    Tree.foreach(this._tree, functor)
    Tree.foreach(other._tree, functor)

    val result = MultiSet[T]()
    result._tree = resultTree
    result._size = resultSize
    result
  }

  def &(other: MultiSet[T]): MultiSet[T] = {
    var resultTree: Tree[T, Int] = Leaf
    var resultSize: Int = 0

    Tree.foreach(this._tree, {
      (key: T, _: Int) =>
        Tree.find(resultTree, key) match {
          case Node(_, _, _, _, _) =>
          case Leaf => (Tree.find(this._tree, key), Tree.find(other._tree, key)) match {
            case (Node(_, _, value1, _, _), Node(_, _, value2, _, _)) =>
              resultSize = resultSize + min(value1, value2)
              resultTree = Tree.insert(resultTree, key, _rand.nextInt(), min(value1, value2))
            case _ =>
          }
        }
    })

    val result = MultiSet[T]()
    result._tree = resultTree
    result._size = resultSize
    result
  }

  def foreach[U](f: T => U): Unit =
    Tree.foreach(_tree, {
      (value: T, count: Int) =>
        for (_ <- 1 to count) {
          f(value)
        }
    })

  def map[U: Ordering](f: T => U): MultiSet[U] = {
    val result = MultiSet[U]()
    foreach { element => result.add(f(element)) }
    result
  }
}


