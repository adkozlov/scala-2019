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
}