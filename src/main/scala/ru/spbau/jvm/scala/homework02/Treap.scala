package ru.spbau.jvm.scala.homework02

import scala.util.Random

abstract class Treap[+K] {

  def foreach[M >: K](f: (M, Int) => Unit)(implicit ord: M => Ordered[M]): Treap[M] = this match {
    case Nul => Nul
    case Node(k, v, p, l, r) =>
      l.foreach(f)
      f(k, v)
      r.foreach(f)
      this
  }

  def toSting[M >: K]()(implicit ord: M => Ordered[M]): String = {
    var string = ""
    this.foreach((key, value) => string += s"[$key, $value] ")
    string.trim()
  }

  def size[M >: K]()(implicit ord: M => Ordered[M]): Int = {
    var size = 0
    this.foreach((key, value) => size += value)
    size
  }

  def findAndApply[M >: K, T](key: M, f: Treap[M] => T)(implicit ord: M => Ordered[M]): T = this match {
    case Nul => f(Nul)
    case Node(k, v, p, l, r) if key == k => f(this)
    case Node(k, v, p, l, r) if key < k => l.findAndApply(key, f)
    case Node(k, v, p, l, r) if key > k => r.findAndApply(key, f)
  }

  def find[M >: K](key: M)(implicit ord: M => Ordered[M]): Treap[M] = {
    def f(t: Treap[M]): Treap[M] = t
    findAndApply(key, f)
  }

  def count[M >: K](key: M)(implicit ord: M => Ordered[M]): Int = {
    def f(t: Treap[M]): Int = t match {
      case Nul => 0
      case Node(k, v, p, l, r) => v
    }
    findAndApply(key, f)
  }

  def contains[M >: K](key: M)(implicit ord: M => Ordered[M]): Boolean = {
      def f(t: Treap[M]): Boolean = t match {
        case Nul => false
        case Node(k, v, p, l, r) => true
      }
      findAndApply(key, f)
  }

  def |[M >: K](that: Treap[M])(implicit ord: M => Ordered[M]): Treap[M] = {
    var union = Treap.empty[M]
    this.foreach((key, value) => union = union.ins(key, value))
    that.foreach((key, value) => union = union.ins(key, value))
    union
  }

  def &[M >: K](that: Treap[M])(implicit ord: M => Ordered[M]): Treap[M] = {
    var intersect = Treap.empty[M]
    this.foreach((key, value) => {
      val count = Math.min(value, that.count(key))
      if (count > 0)
        intersect = intersect.ins(key, count)
    })
    intersect
  }

  def insert[M >: K](key: M)(implicit ord: M => Ordered[M]): Treap[M] = this.ins(key, 1)

  def delete[M >: K](key: M)(implicit ord: M => Ordered[M]): Treap[M] = this.del(key, 1)

  private def split[M >: K](t: Treap[M], key: M)(implicit ord: M => Ordered[M]): (Treap[M], Treap[M]) = t match {
    case Nul => (Nul, Nul)
    case Node(k, v, p, l, r) =>
      if (key == k)
        return (l, r)
      if (key < k) {
        val (ll, rr) = split(l, key)
        (ll, Node(k, v, p, rr, r))
      } else {
        val (ll, rr) = split(r, key)
        (Node(k, v, p, l, ll), rr)
      }
  }

  private def merge[M >: K](t1: Treap[M], t2: Treap[M])(implicit ord: M => Ordered[M]): Treap[M] = (t1, t2) match {
    case (t, Nul) => t
    case (Nul, t) => t
    case (Node(k1, v1, p1, l1, r1), Node(k2, v2, p2, l2, r2)) =>
      if (p1 > p2)
        Node(k1, v1, p1, l1, merge(r1, t2))
      else
        Node(k2, v2, p2, merge(t1, l2), r2)
  }

  private def ins[M >: K](key: M, value: Int, prior: Int = Treap.genPrior)(implicit ord: M => Ordered[M]): Treap[M] = find(key) match {
    case node: Node[M] =>
      node.value += value
      this
    case _ =>
      val (t1, t2) = split(this, key)
      merge(merge(t1, Node(key, value, prior, Nul, Nul)), t2)
  }

  private def del[M >: K](key: M, value: Int)(implicit ord: M => Ordered[M]): Treap[K] = this match {
    case Nul => Nul
    case Node(k, v, p, l, r) if key < k => Node(k, v, p, l.del(key, value), r)
    case Node(k, v, p, l, r) if key > k => Node(k, v, p, l, r.del(key, value))
    case Node(k, v, p, l, r) if key == k && v > value => {
      Node(k, v - value, p, l, r)
    }
    case Node(k, v, p, l, r) if key == k && v <= value => {
      merge(l, r)
    }
  }
}

private case object Nul extends Treap[Nothing]

private case class Node[K](var key: K,
                           var value: Int,
                           var prior: Int,
                           var left: Treap[K],
                           var right: Treap[K])(implicit ord: Ordering[K]) extends Treap[K]

object Treap {

  private def genPrior: Int = random.nextInt()

  private val random: Random = new Random(30)

  def empty[K](implicit ord: Ordering[K]): Treap[K] = Nul

  def apply[K](args: K*)(implicit ord: Ordering[K]): Treap[K] = {
    var tree: Treap[K] = Nul
    for (k <- args) {
      tree = tree.insert(k)
    }
    tree
  }
}

