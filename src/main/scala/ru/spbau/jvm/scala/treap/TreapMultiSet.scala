package ru.spbau.jvm.scala.treap

import java.lang.StringBuilder

class TreapMultiSet[K] private (val root: Treap[K])(implicit ord: Ordering[K]) {
  import Treap._

  def this(keys: K*)(implicit ord: Ordering[K]) = this({
    var root: Treap[K] = EmptyNode()
    for (k <- keys) {
      root = Treap.addInsert(root, k, 1)(ord)
    }
    root
  })

  def count(key: K): Int = {
    val (lowerEqual, _) = split(root, key)
    val (_, mightBeEqual) = splitRightest(lowerEqual)
    mightBeEqual.map(c => if (ord.equiv(key, c.key)) c.number else 0).getOrElse(0)
  }

  def foreach: (K => Unit) => Unit = root.foreach
  def foreachOnce: ((K, Int) => Unit) => Unit = root.foreachOnce

  def |(that: TreapMultiSet[K]): TreapMultiSet[K] = {
    val (addTo, addFrom) = smallerBigger(that)
    var resultRoot: Treap[K] = addTo.root
    addFrom.foreachOnce((element, count) => {
      val addToCount = addTo.count(element)
      if (addToCount < count)
        resultRoot = addInsert(resultRoot, element, count - addToCount)
    })
    new TreapMultiSet(resultRoot)
  }

  def &(that: TreapMultiSet[K]): TreapMultiSet[K] = {
    val (smaller, bigger) = smallerBigger(that)
    var resultRoot: Treap[K] = EmptyNode()
    smaller.foreachOnce((element, count) => {
      val biggerCount = bigger.count(element)
      if (biggerCount != 0)
        resultRoot = addInsert(resultRoot, element, Math.min(count, biggerCount))
    })
    new TreapMultiSet(resultRoot)
  }

  def map[K2](f: K => K2)(implicit ord2: Ordering[K2]): TreapMultiSet[K2] = {
    var resultRoot: Treap[K2] = EmptyNode()
    foreachOnce((element, count) => {
      resultRoot = addInsert(resultRoot, f(element), count)
    })
    new TreapMultiSet(resultRoot)
  }

  def withFilter(f: K => Boolean): TreapMultiSet[K] = {
    var resultRoot: Treap[K] = EmptyNode()
    foreachOnce((element, count) => {
      if (f(element)) resultRoot = addInsert(resultRoot, element, count)
    })
    new TreapMultiSet(resultRoot)
  }

  override def toString: String = {
    val resultBuilder = new StringBuilder
    val separator = ", "
    resultBuilder.append("[")
    root.foreachOnce((element, count) => resultBuilder.append(s"$element -> $count").append(separator))
    resultBuilder.setLength(resultBuilder.length() - separator.length)
    resultBuilder.append("]")
    resultBuilder.toString
  }

  private def smallerBigger(that: TreapMultiSet[K]): (TreapMultiSet[K], TreapMultiSet[K]) = {
    if (this.root.size < that.root.size)
      (this, that)
    else
      (that, this)
  }
}

object TreapMultiSet {
  def apply[K](keys: K*)(implicit ord: Ordering[K]) = new TreapMultiSet(keys:_*)
}