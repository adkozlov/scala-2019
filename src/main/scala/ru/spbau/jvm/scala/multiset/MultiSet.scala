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

  private def toList: List[V] = {
    var result: List[V] = Nil
    tree.toList.foreach(value =>
      for (_ <- 1 to value._2) {
        result = result ::: (value._1 :: Nil)
      }
    )
    result
  }

  def map[U](function: V => U)(implicit ord: U => Ordered[U]): MultiSet[U] = {
    MultiSet(toList.map(function))
  }

  def filter(predicate: V => Boolean)(implicit ord: V => Ordered[V]): MultiSet[V] = {
    MultiSet(toList.filter(predicate))
  }

  def withFilter(predicate: V => Boolean): MultiSet[V] = {
    MultiSet(toList.filter(predicate))
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
    MultiSet(toList ::: other.toList)
  }

  def &(other: MultiSet[V]): MultiSet[V] = {
    MultiSet(toList.filter(x => other.contains(x)) ::: other.toList.filter(x => contains(x)))
  }

  override def toString: String = {
    var result = "["
    tree.toList.map(x => s"${x._1} -> ${x._2}").foreach(value => result += value + ", ")
    if (result.length > 1) {
      result = result.dropRight(2)
    }
    result += "]"
    result
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: MultiSet[V] =>
      var result = true
      for (elem <- other.toList) {
        result = result && contains(elem) && (other(elem) == this (elem))
      }
      result
    case _ => false
  }
}

object MultiSet {
  def apply[U]()(implicit ord: U => Ordered[U]) = new MultiSet[U]()

  def apply[U](values: U*)(implicit ord: U => Ordered[U]): MultiSet[U] = {
    val result = MultiSet[U]()
    values.foreach((value) => result.add(value))
    result
  }

  def apply[U](values: List[U])(implicit ord: U => Ordered[U]): MultiSet[U] = {
    val result = new MultiSet[U]
    values.foreach(result.add)
    result
  }

  def empty[U](implicit ord: U => Ordered[U]): MultiSet[U] = MultiSet[U]()
}