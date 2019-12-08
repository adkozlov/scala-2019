package ru.spbau.jvm.scala

import scala.collection.IterableOnce
import scala.util.Random

class MultiSet[V](private val count: Int = 0, private val tree: Tree[V, Int] = Leaf[V, Int]())(implicit ord: Ordering[V]) {
  def size(): Int = count

  def isEmpty: Boolean = count == 0

  def contains(value: V): Boolean = tree.find(value).isDefined

  def add(value: V): MultiSet[V] = {
    tree.find(value) match {
      case Some(node) =>
        MultiSet(count + 1, tree.erase(value).insert(Node(node.key, node.value + 1, node.priority, node.size,
          node.leftChild, node.rightChild)))
      case None => MultiSet(count + 1, tree.insert(Node(value, 1, Random.nextInt(), 1, Leaf(), Leaf())))
    }
  }

  def remove(value: V): MultiSet[V] = {
    tree.find(value) match {
      case Some(node) =>
        if (node.value == 1) MultiSet[V](count - 1, tree.erase(value)) else MultiSet[V](count - 1,
          tree.erase(value).insert(Node(node.key, node.value - 1, node.priority, node.size,
            node.leftChild, node.rightChild)))
      case None => this
    }
  }

  def clear(): MultiSet[V] = {
    MultiSet()
  }

  private def toMultiList: List[V] = {
    tree.toList.flatMap(p => List.fill(p._2)(p._1))
  }

  def iterator: Iterator[V] = toMultiList.iterator

  def map[U](f: V => U)(implicit ord: Ordering[U]): MultiSet[U] = {
    MultiSet(toMultiList.map(f))
  }

  def foreach[U](f: V => U): Unit = {
    toMultiList.foreach(f)
  }

  def filter(p: V => Boolean): MultiSet[V] = {
    MultiSet(toMultiList.filter(p))
  }

  def withFilter(p: V => Boolean): MultiSet[V] = {
    MultiSet(toMultiList.filter(p))
  }

  def flatMap[U](f : V => IterableOnce[U])(implicit ord: Ordering[U]): MultiSet[U] = {
    MultiSet(toMultiList.flatMap(f))
  }

  def |(other: MultiSet[V]): MultiSet[V] = {
    MultiSet(toMultiList ++ other.toMultiList)
  }

  def &(other: MultiSet[V]): MultiSet[V] = {
    MultiSet(toMultiList.filter(x => other.toMultiList.contains(x)) ++
      other.toMultiList.filter(x => toMultiList.contains(x)))
  }

  def apply(value: V): Int = {
    tree.find(value) match {
      case Some(node) => node.value
      case _          => 0
    }
  }

  override def toString: String = {
    tree.toList.map(p => p._1 + " -> " + p._2).mkString("[", ", ", "]")
  }
}

object MultiSet {
  def apply[V]()(implicit ord: Ordering[V]): MultiSet[V] = MultiSet(0, Leaf[V, Int]())

  def apply[V](count: Int, tree: Tree[V, Int])(implicit ord: Ordering[V]): MultiSet[V] = new MultiSet(count, tree)

  def apply[V](values: List[V])(implicit ord: Ordering[V]): MultiSet[V] = values.foldLeft(MultiSet[V]())((set, value) => set.add(value))

  def apply[V](values: V*)(implicit ord: Ordering[V]): MultiSet[V] = MultiSet[V](values.toList)
}
