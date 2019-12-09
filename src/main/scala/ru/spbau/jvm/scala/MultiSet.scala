package ru.spbau.jvm.scala

object MultiSet {
  def empty[E]()(implicit ordering: Ordering[E]) =
    new MultiSet[E]()

  def apply[E](elems: E*)(implicit ord: Ordering[E]): MultiSet[E] = {
    val ms = empty[E]()
    for (elem <- elems) {
      ms.insert(elem)
    }
    ms
  }

  def apply[E](list: List[(E, Int)])(implicit ord: Ordering[E]): MultiSet[E] = {
    val ms = empty[E]()
    for ((elem, num) <- list) {
      ms.insert(elem, num)
    }
    ms
  }
}

class MultiSet[E](private var tree: Treap[E, Int] = null)(
  implicit ord: Ordering[E]
) {

  def insert(elem: E): MultiSet[E] = insert(elem, 1)

  def insert(elem: E, count: Int): MultiSet[E] = {
    if (count > 0) {
      tree = if (tree == null) {
        new Treap(elem, 1)
      } else {
        val removeElem = tree.remove(elem)
        tree = removeElem._2
        removeElem._1 match {
          case None      => tree.insert(elem, count)
          case Some(pri) => tree.insert(elem, pri + count)
        }
      }
    }
    this
  }

  def remove(elem: E): MultiSet[E] = {
    if (tree != null) {
      tree.remove(elem)._1 match {
        case Some(pri) =>
          if (pri > 0) {
            tree = tree.insert(elem, pri - 1)
          }
      }
    }
    this
  }

  def isEmpty: Boolean = tree == null

  def notEmpty: Boolean = !isEmpty

  def toList: List[(E, Int)] = tree.toList

  def countOf(elem: E): Int = {
    tree.getPri(elem) match {
      case None      => 0
      case Some(pri) => pri
    }
  }

  def &(multiSet: MultiSet[E]): MultiSet[E] = {
    val intersect = new MultiSet[E]()
    for ((elem, num) <- multiSet.toList) {
      val minCount = countOf(elem).min(num)
      if (minCount > 0) {
        intersect.insert(elem, minCount)
      }
    }
    intersect
  }

  def |(multiSet: MultiSet[E]): MultiSet[E] = {
    val union = new MultiSet[E]()
    for ((elem, num) <- multiSet.toList) {
      union.insert(elem, num)
    }
    for ((elem, num) <- toList) {
      union.insert(elem, num)
    }
    union
  }

  def foreach[R](f: E => R): Unit = tree.keyIterator().foreach(f)

  def map[R](f: E => R)(implicit ord: Ordering[R]): MultiSet[R] = {
    MultiSet(tree.toList.map(x => (f(x._1), x._2)))
  }

//  def filter(p: E => Boolean): MultiSet[E] = {
//    MultiSet(tree.toList.filter(x => if p(x._1) x else (x._1, 0)))
//  }

  override def toString: String = {
    tree.toList.map(p => p._1 + " -> " + p._2).mkString("[", ", ", "]")
  }
}
