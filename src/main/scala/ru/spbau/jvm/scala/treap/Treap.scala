package ru.spbau.jvm.scala.treap

import ru.spbau.jvm.scala.lecture03.List

class NodeContent[K](val key: K, val number: Int, val priority: Int)

object NodeContent {
  def apply[K](key: K, number: Int, priority: Int): NodeContent[K] = new NodeContent[K](key, number, priority)
  def apply[K](key: K, number: Int): NodeContent[K] = new NodeContent[K](key, number, random.nextInt())
  private val random = scala.util.Random
}

sealed trait Treap[K] {
  def base: NodeContent[K]
  def left: Treap[K]
  def right: Treap[K]
  def nodeSize: Int
  def isEmpty: Boolean
  def foreach(f: K => Unit): Unit
  def foreachOnce(f: (K, Int) => Unit): Unit
}

case class TreapNode[K](base: NodeContent[K], left: Treap[K], right: Treap[K])(implicit ord:Ordering[K]) extends Treap[K] {
  private val size = left.nodeSize + right.nodeSize + 1
  override def nodeSize: Int = size
  override def isEmpty: Boolean = false

  override def foreach(f: K => Unit): Unit = {
    left.foreach(f)
    for (_ <- 1 to base.number) f(base.key)
    right.foreach(f)
  }

  override def foreachOnce(f: (K, Int) => Unit): Unit = {
    left.foreachOnce(f)
    f(base.key, base.number)
    right.foreachOnce(f)
  }
}

case class EmptyNode[K]() extends Treap[K] {
  override def base = throw new NoSuchElementException
  override def left = throw new UnsupportedOperationException
  override def right = throw new UnsupportedOperationException
  override def nodeSize: Int = 0
  override def isEmpty: Boolean = true
  override def foreach(f: K => Unit): Unit = {}
  override def foreachOnce(f: (K, Int) => Unit): Unit = {}
}

object EmptyNode {
  def apply[K](): EmptyNode[K] = new EmptyNode()
}

object TreapNode {
  def apply[K](base: NodeContent[K], left: Treap[K], right: Treap[K])(implicit ord:Ordering[K]): TreapNode[K] = new TreapNode(base, left, right)
  def apply[K](base: NodeContent[K])(implicit ord:Ordering[K]): TreapNode[K] = new TreapNode(base, EmptyNode(), EmptyNode())
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
      new TreapNode[K](base, Treap(content.filter(p => ord.lt(p.key, base.key))), Treap(content.filter(p => ord.gt(p.key, base.key))))
    }
  }
}

object Treap {
  def apply[K](content: List[NodeContent[K]])(implicit ord:Ordering[K]): Treap[K] = if (content.isEmpty) EmptyNode() else TreapNode(content)
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
    case EmptyNode() => (EmptyNode(), EmptyNode())
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
    case EmptyNode() => (EmptyNode(), Option.empty)
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
