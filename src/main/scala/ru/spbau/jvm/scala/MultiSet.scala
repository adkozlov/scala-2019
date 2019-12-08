package ru.spbau.jvm.scala

final class MultiSet[T](val tree: AvlTree[T])
                       (implicit ordering: Ordering[T]) {

  def +(elem: T): MultiSet[T] = new MultiSet[T](tree.add(elem))

  def -(elem: T): MultiSet[T] = new MultiSet[T](tree.remove(elem))

  def add(elem: T, count: Int) = new MultiSet[T](tree.add(elem, count))

  def foreach(f: T => Unit): Unit = tree.foreach(f)

  def isEmpty: Boolean = tree.isEmpty

  def notEmpty: Boolean = tree.notEmpty

  def &(other: MultiSet[T]): MultiSet[T] = {
    var tree: AvlTree[T] = AvlNil[T]()
    for (elem <- other) {
      if (contains(elem)) {
        if (tree.contains(elem))
          tree = tree.add(elem, 1)
        else
          tree = tree.add(elem, 1 + this.tree(elem))
      }
    }
    new MultiSet[T](tree)
  }

  def map[U](f: T => U)(implicit o: Ordering[U]): MultiSet[U] = {
    var tree: AvlTree[U] = AvlNil[U]()
    for (elem <- this.tree) {
      tree = tree.add(f(elem))
    }
    new MultiSet[U](tree)
  }

  def flatMap(f: T => MultiSet[T]): MultiSet[T] = {
    var set = MultiSet[T]()
    for (elem <- tree) {
      set = set | f(elem)
    }
    set
  }

  def |(other: MultiSet[T]): MultiSet[T] = {
    var newTree = tree
    for (elem <- other) {
      newTree = newTree.add(elem)
    }
    new MultiSet[T](newTree)
  }

  // Would be more efficient with use with WithFilter, but it'd be just
  // a matter of copying the implementation of standard library.
  def withFilter(pred: T => Boolean): MultiSet[T] = filter(pred)

  def filter(pred: T => Boolean): MultiSet[T] = {
    var tree: AvlTree[T] = AvlNil[T]()
    for (elem <- this.tree) {
      if (pred(elem)) {
        tree = tree.add(elem)
      }
    }
    new MultiSet[T](tree)
  }

  def fold[U](acc: U)(f: (T, U) => U): U = {
    var result = acc
    for (elem <- tree) {
      result = f(elem, result)
    }
    result
  }

  def apply(elem: T): Int = tree(elem)

  override def toString: String = s"[${tree.toString}]"

  override def equals(o: Any): Boolean = o match {
    case other: MultiSet[T] =>
      var result = true
      for (elem <- other) {
        result = result && contains(elem) && (other(elem) == this (elem))
      }
      result
    case _ => false
  }

  def contains(elem: T): Boolean = tree.contains(elem)
}

object MultiSet {
  def apply[T](elems: T*)(implicit ordering: Ordering[T]): MultiSet[T] = {
    var set = empty[T]()
    for (elem <- elems) {
      set = set + elem
    }
    set
  }

  def empty[T]()(implicit ordering: Ordering[T]) = new MultiSet[T](AvlNil[T]())
}
