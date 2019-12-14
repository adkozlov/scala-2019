package ru.spbau.jvm.scala.homework02

class MultiSet[K](implicit val ord: Ordering[K]) {

  private var treap: Treap[K] = Treap.empty

  def insert(key: K): Unit = treap = treap.insert(key)

  def remove(key: K): Unit = treap = treap.delete(key)

  def foreach(f: (K, Int) => Unit): Unit = treap.foreach(f)

  override def toString: String = treap.toSting()

  def find(key: K): Option[(K, Int)]
  = treap.find(key) match {
    case Node(k, v, p, l, r) => Option((k, v))
    case _ => Option.empty
  }

  def |(that: MultiSet[K]): MultiSet[K] = {
    val union = MultiSet.empty
    union.treap = treap | that.treap
    union
  }

  def &(that: MultiSet[K]): MultiSet[K] = {
    val intersect = MultiSet.empty
    intersect.treap = treap & that.treap
    intersect
  }

  def count(key: K): Int = treap.count(key)

  def size: Int = treap.size

  def contains(key: K): Boolean = treap.contains(key)
}

object MultiSet {
  def empty[K](implicit ordering: Ordering[K]): MultiSet[K] = new MultiSet[K]

  def apply[K](args : K*)(implicit ordering: Ordering[K]): MultiSet[K] = {
    val multiSet = new MultiSet[K]
    for (k <- args) {
      multiSet.insert(k)
    }
    multiSet
  }
}