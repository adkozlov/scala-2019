package ru.spbau.jvm.scala.treap

import ru.spbau.jvm.scala.lecture03.List

case class NodeContent[+K] private (key: K, number: Int, priority: Int)

object NodeContent {
  def apply[K](key: K, number: Int, priority: Int = random.nextInt()): NodeContent[K] = new NodeContent[K](key, number, priority)
  private val random = scala.util.Random
}

sealed trait Treap[+K] {
  def base: NodeContent[K]
  def left: Treap[K]
  def right: Treap[K]
  val size: Int
  def isEmpty: Boolean
  def foreachOnce(f: (K, Int) => Unit): Unit
  def foreach(f: K => Unit): Unit = foreachOnce {
    case (key, number) => for (_ <- 1 to number) f(key)
  }
}

case class TreapNode[+K](base: NodeContent[K], left: Treap[K], right: Treap[K])(implicit ord:Ordering[K]) extends Treap[K] {
  override val size: Int = left.size + right.size + 1
  override def isEmpty: Boolean = false

  override def foreachOnce(f: (K, Int) => Unit): Unit = {
    left.foreachOnce(f)
    f(base.key, base.number)
    right.foreachOnce(f)
  }
}

case object EmptyNode extends Treap[Nothing] {
  override def base = throw new NoSuchElementException
  override def left = throw new UnsupportedOperationException
  override def right = throw new UnsupportedOperationException
  override val size: Int = 0
  override def isEmpty: Boolean = true
  override def foreachOnce(f: (Nothing, Int) => Unit): Unit = {}
}

object TreapNode {
  def apply[K](base: NodeContent[K], left: Treap[K], right: Treap[K])(implicit ord:Ordering[K]): TreapNode[K] = new TreapNode(base, left, right)
  def apply[K](base: NodeContent[K])(implicit ord:Ordering[K]): TreapNode[K] = new TreapNode(base, EmptyNode, EmptyNode)
  def apply[K](content: List[NodeContent[K]])(implicit ord:Ordering[K]): TreapNode[K] = {
    var minElement: Option[NodeContent[K]] = Option.empty
    for (x <- content) {
      if (minElement.forall(_.priority > x.priority))
        minElement = Option(x)
    }
    if (minElement.isEmpty)
      throw new IllegalArgumentException("empty list provided to a TreapNode")
    else {
      val base = minElement.get
      new TreapNode[K](base, Treap(content.withFilter(p => ord.lt(p.key, base.key))), Treap(content.withFilter(p => ord.gt(p.key, base.key))))
    }
  }
}

object Treap {
  def apply[K](content: List[NodeContent[K]])(implicit ord:Ordering[K]): Treap[K] = if (content.isEmpty) EmptyNode else TreapNode(content)
  def unapply[K](arg: Treap[K]): Option[(NodeContent[K], Treap[K], Treap[K])] = if (arg.isEmpty) Option.empty else Option(arg.base, arg.left, arg.right)

  def merge[K](left: Treap[K], right: Treap[K])(implicit ord:Ordering[K]): Treap[K] = {
    if (left.isEmpty) return right
    if (right.isEmpty) return left

    if (left.base.priority > right.base.priority)
      new TreapNode(left.base, left.left, merge(left.right, right))
    else
      new TreapNode(right.base, merge(left, right.left), right.right)
  }

  def split[K](root: Treap[K], middle: K)(implicit ord:Ordering[K]): (Treap[K], Treap[K]) = root match {
    case EmptyNode => (EmptyNode, EmptyNode)
    case TreapNode(base, left, right) =>
      if (ord.lteq(base.key, middle)) {
        val (l, r) = split(right, middle)
        (new TreapNode(base, left, l), r)
      } else {
        val (l, r) = split(left, middle)
        (l, new TreapNode(base, r, right))
      }
  }

  def splitRightest[K](root: Treap[K])(implicit ord:Ordering[K]): (Treap[K], Option[NodeContent[K]]) = root match {
    case EmptyNode => (EmptyNode, Option.empty)
    case TreapNode(base, left, right) =>
      if (right.isEmpty)
        (left, Option(base))
      else {
        val (r, rightest) = splitRightest(right)
        (new TreapNode(base, left, r), rightest)
      }
  }

  def addInsert[K](root: Treap[K], key: K, addNumber: Int)(implicit ord:Ordering[K]): Treap[K] = {
    val (lowerOrEqual, greater) = split(root, key)
    val (lower, lowerMaybeEqual) = splitRightest(lowerOrEqual)
    val (l, equalContent) = lowerMaybeEqual.map(content =>
      if (content.key == key)
        (lower, NodeContent(key, content.number + addNumber, content.priority))
      else
        (lowerOrEqual, NodeContent(key, addNumber))
    ).getOrElse((lowerOrEqual, NodeContent(key, addNumber)))
    val equal = TreapNode(equalContent)
    merge(l, merge(equal, greater))
  }
}
