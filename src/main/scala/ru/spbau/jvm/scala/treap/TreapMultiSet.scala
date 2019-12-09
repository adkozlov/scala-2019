package ru.spbau.jvm.scala.treap

class TreapMultiSet[K] private (val root: Treap[K])(implicit ord: Ordering[K]) {
  import Treap._

  def this(keys: K*)(implicit ord: Ordering[K]) = this({
    var contents: Seq[NodeContent[K]] = Seq.empty
    var last: Option[K] = Option.empty
    var lastCount = 0
    for (k <- keys.sorted(ord).reverse) {
      if (last.contains(k))
        lastCount += 1
      else {
        last.foreach(lastKey => contents = NodeContent(lastKey, lastCount) +: contents)
        last = Option(k)
        lastCount = 1
      }
    }
    last.foreach(lastKey => contents = NodeContent(lastKey, lastCount) +: contents)
    Treap(contents)(ord)
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

  private def smallerBigger(that: TreapMultiSet[K]): (TreapMultiSet[K], TreapMultiSet[K]) = {
    if (this.root.nodeSize < that.root.nodeSize)
      (this, that)
    else
      (that, this)
  }
}