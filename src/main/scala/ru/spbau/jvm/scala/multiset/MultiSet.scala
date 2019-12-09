package ru.spbau.jvm.scala.multiset

class MultiSet[A <: Ordered[A]] {
  private val treap: Treap[A] = new Treap[A]
  private var size = 0

  def this(e: A) {
    this()
    add(e)
  }

  def this(e: scala.collection.Iterable[A]) {
    this()
    e.foreach(value => add(value))
  }

  def this(e: A*) {
    this()
    e.foreach(value => add(value))
  }

  def getSize: Int = {
    return size
  }

  def add(e: A) {
    treap.add(e)
    size += 1
  }

  def add(e: A, count: Int): Unit = {
    if (count > 0) {
      treap.add(e, count)
      size += count
    }
  }

  def remove(e: A): Unit = {
    if (treap.remove(e)) {
      size -= 1
    }
  }

  def contains(e: A): Boolean = {
    treap.get(e).isDefined
  }

  def apply(e: A): Int = {
    val node = treap.get(e)
    if (node.isDefined) {
      return node.get.count
    } else {
      return 0
    }
  }

  def empty(): Boolean = {
    return treap.empty()
  }

  def &(that: MultiSet[A]): MultiSet[A] = {
    val result = new MultiSet[A]
    foreachCount((value, count) => {
      val thatCount: Int = that.apply(value)
      if (thatCount > 0) {
        result.add(value, math.min(count, thatCount))
      }
    })

    return result
  }

  def |(that: MultiSet[A]): MultiSet[A] = {
    val result = new MultiSet[A]
    this.foreachCount((value, count) => result.add(value, count))
    that.foreachCount((value, count) => result.add(value, count))
    return result
  }

  def foreachCount[U](f: (A, Int) => U): Unit = {
    treap.foreachCount(f)
  }

  def foreach[U](f: A => U): Unit = {
    treap.foreach(f)
  }

  def map[B <: Ordered[B]](f: (A) => B): MultiSet[B] = {
    val result = new MultiSet[B]
    foreachCount((value, count) => result.add(f(value), count))
    return result
  }

  def withFilter(p: (A) => Boolean): MultiSet[A] = {
    val result = new MultiSet[A]
    foreachCount((value, count) => if (p(value)) result.add(value, count))
    return result
  }

  override def toString: String = {
    var result: Seq[String] = Seq.empty
    foreachCount((value, count) => result = result :+ s"$value -> $count")
    result.mkString("[", ", ", "]")
  }
}