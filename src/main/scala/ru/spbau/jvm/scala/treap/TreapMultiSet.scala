package ru.spbau.jvm.scala.treap

class TreapMultiSet[K](keys: K*)(implicit canCompare: K => Ordered[K]) {

  def root = {
    var contents: Seq[NodeContent] = Seq.empty
    var last: Option[K] = Option.empty
    var lastCount = 0
    for (k <- keys.sorted.reverse) {
      if (last.contains(k))
        lastCount += 1
      else {
        last.foreach(lastKey => contents = NodeContent(lastKey, lastCount) +: contents)
        last = Option(k)
        lastCount = 1
      }
    }
    last.foreach(lastKey => contents = NodeContent(lastKey, lastCount) +: contents)
    Treap(contents)
  }

  class NodeContent(val key: K, val number: Int, val priority: Int)

  object NodeContent {
    def apply(key: K, number: Int, priority: Int): NodeContent = new NodeContent(key, number, priority)
    def apply(key: K, number: Int): NodeContent = new NodeContent(key, number, random.nextInt())
    private val random = scala.util.Random
  }

  sealed trait Treap {
    def base: NodeContent
    def left: Treap
    def right: Treap
    def nodeSize: Int
  }

  case class TreapNode(base: NodeContent, left: Treap, right: Treap) extends Treap {
    private val size = left.nodeSize + right.nodeSize + 1
    override def nodeSize: Int = size
  }

  case object EmptyNode extends Treap {
    override def base = throw new NoSuchElementException
    override def left = throw new UnsupportedOperationException
    override def right = throw new UnsupportedOperationException
    override def nodeSize: Int = 0
  }

  object TreapNode {
    def apply(base: NodeContent, left: Treap = EmptyNode, right: Treap = EmptyNode): TreapNode = new TreapNode(base, left, right)
    def apply(content: Seq[NodeContent]): TreapNode = {
      val base = content.minBy(_.priority)
      TreapNode(base, TreapNode(content.filter(_.key < base.key)), TreapNode(content.filter(_.key > base.key)))
    }
  }

  object Treap {
    def apply(content: Seq[NodeContent]): Treap = if (content.isEmpty) EmptyNode else TreapNode(content)
    def unapply(arg: Treap): Option[(NodeContent, Treap, Treap)] = if (arg == EmptyNode) Option.empty else Option(arg.base, arg.left, arg.right)

    def merge(left: Treap, right: Treap): Treap = {
      if (left == EmptyNode) return right
      if (right == EmptyNode) return left

      if (left.base.priority > right.base.priority)
        TreapNode(left.base, left.left, merge(left.right, right))
      else
        TreapNode(right.base, merge(left, right.left), right.right)
    }

    def split(root: Treap, middle: K): (Treap, Treap) = root match {
      case EmptyNode => (EmptyNode, EmptyNode)
      case TreapNode(base, left, right) =>
        if (base.key <= middle) {
          val (l, r) = split(right, middle)
          (TreapNode(base, left, l), r)
        } else {
          val (l, r) = split(left, middle)
          (l, TreapNode(base, r, right))
        }
    }

    def splitRightest(root: Treap): (Treap, Option[NodeContent]) = root match {
      case EmptyNode => (EmptyNode, Option.empty)
      case TreapNode(base, left, right) =>
        if (right == EmptyNode)
          (left, Option(base))
        else {
          val (r, rightest) = splitRightest(right)
          (TreapNode(base, left, r), rightest)
        }
    }

    def addInsert(root: Treap, key: K, addNumber: Int): Treap = {
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
}