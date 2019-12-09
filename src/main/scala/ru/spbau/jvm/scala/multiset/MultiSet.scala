package ru.spbau.jvm.scala.multiset

class MultiSet[V](implicit ord: V => Ordered[V]) {
  private val tree = new Treap[V, Integer]
  private var setSize: Int = 0

  def add(elem: V): MultiSet[V] = {
    val prevValue = tree.get(elem)
    if (prevValue == null) {
      tree.add(elem, 1)
    } else {
      tree.add(elem, prevValue + 1)
    }
    setSize += 1
    this
  }

  def get(elem: V): Int = {
    this (elem)
  }

  def remove(elem: V): MultiSet[V] = {
    val prevValue = tree.get(elem)
    if (prevValue != null) {
      if (prevValue > 0) {
        tree.changeValue(elem, prevValue - 1)
      } else {
        tree.remove(elem)
      }
      setSize -= 1
    }
    this
  }

  def size(): Int = setSize

  private def toList: List[V] =
    tree.toList.flatMap(value => List.range(1, value._2 + 1).map(_ => value._1))

  def map[U](function: V => U)(implicit ord: U => Ordered[U]): MultiSet[U] = {
    MultiSet(toList.map(function))
  }

  def filter(predicate: V => Boolean)(implicit ord: V => Ordered[V]): MultiSet[V] = {
    MultiSet(toList.filter(predicate))
  }

  def withFilter(predicate: V => Boolean): MultiSet[V] = {
    MultiSet(toList.filter(predicate))
  }

  def flatMap[U](func: V => IterableOnce[U])(implicit ord: U => Ordered[U]): MultiSet[U] = {
    MultiSet(toList.flatMap(func))
  }

  def apply(elem: V): Int = {
    val result = tree.get(elem)
    if (result == null) {
      0
    } else {
      result
    }
  }

  def contains(elem: V): Boolean = {
    tree.get(elem) != null
  }

  def |(other: MultiSet[V]): MultiSet[V] = {
    MultiSet(toList ++ other.toList)
  }

  def &(other: MultiSet[V]): MultiSet[V] = {
    MultiSet(toList.filter(x => other.contains(x)) ++ other.toList.filter(x => contains(x)))
  }

  override def toString: String =
    "[" + tree.toList.map(x => s"${x._1} -> ${x._2}").mkString(", ") + "]"

  override def equals(obj: Any): Boolean = obj match {
    case other: MultiSet[V] =>
      var result = true
      for (elem <- other.toList) {
        result = result && contains(elem) && (other(elem) == this (elem))
      }
      result
    case _ => false
  }

  def iterator(): Iterator[(V, Integer)] = tree.iterator()
}

object MultiSet {
  def apply[U]()(implicit ord: U => Ordered[U]) = new MultiSet[U]()

  def apply[U](values: U*)(implicit ord: U => Ordered[U]): MultiSet[U] = MultiSet[U](values.toList)

  def apply[U](values: List[U])(implicit ord: U => Ordered[U]): MultiSet[U] = {
    val result = new MultiSet[U]
    values.foreach(result.add)
    result
  }

  def empty[U](implicit ord: U => Ordered[U]): MultiSet[U] = MultiSet[U]()
}