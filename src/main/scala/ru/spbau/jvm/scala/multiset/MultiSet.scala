package ru.spbau.jvm.scala.multiset

sealed trait Tree[+A] {

  def head: Node[A]

  def left: Tree[A]

  def right: Tree[A]

  def height: Int

  private def smallRightRot[B](head: Node[B], left: Tree[B], right: Tree[B]): Tree[B] = {
    val newRight = Cons(head, left.right, right)
    Cons(left.head, left.left, newRight)
  }

  private def bigRightRot[B](head: Node[B], left: Tree[B], right: Tree[B]): Tree[B] = {
    val newRightLeft = left.right.right
    val newRightRight = right
    val newRight = Cons(head, newRightLeft, newRightRight)
    val newLeftLeft = left.left
    val newLeftRight = left.right.left
    val newLeft = Cons(left.head, newLeftLeft, newLeftRight)
    val newHead = left.right.head
    Cons(newHead, newLeft, newRight)
  }

  private def smallLeftRot[B](head: Node[B], left: Tree[B], right: Tree[B]): Tree[B] = {
    val newLeftLeft = left
    val newLeftRight = right.left
    val newLeft = Cons(head, newLeftLeft, newLeftRight)
    Cons(right.head, newLeft, right.right)
  }

  private def bigLeftRot[B](head: Node[B], left: Tree[B], right: Tree[B]): Tree[B] = {
    val newLeftLeft = left
    val newLeftRight = right.left.left
    val newLeftHead = head
    val newLeft = Cons(newLeftHead, newLeftLeft, newLeftRight)
    val newRightLeft = right.left.right
    val newRightRight = right.right
    val newRightHead = right.head
    val newRight = Cons(newRightHead, newRightLeft, newRightRight)
    val newHead = right.left.head
    Cons(newHead, newLeft, newRight)
  }

  def balanced[B](head: Node[B], left: Tree[B], right: Tree[B]): Tree[B] = {
    if (left.height > right.height + 1) {
      if (left.left.height >= left.right.height) {
        smallRightRot(head, left, right)
      } else {
        bigRightRot(head, left, right)
      }
    }
    if (left.height + 1 < right.height) {
      if (right.right.height >= right.left.height) {
        smallLeftRot(head, left, right)
      } else {
        bigLeftRot(head, left, right)
      }
    }
    Cons(head, left, right)
  }

  def apply[B >: A](elem: B): Int

  def +[B >: A](elem: B): Tree[B] = {
    add(elem, 1)
  }

  def add[B >: A](elem: B, count: Int): Tree[B]

  def remove[B >: A](elem: B, count: Int): Tree[B]

  def |[B >: A](other: Tree[B]): Tree[B]

  def &[B >: A](other: Tree[B]): Tree[B]

  def contains(elem: Any): Boolean

  def min: Node[A]

  def max: Node[A]

  def isEmpty: Boolean

  def iterator: Iterator[Node[A]]

  def foreach(function: A => Unit): Unit

}

class Node[+A](val key: A, val count: Int)

case class Cons[+A](head: Node[A], left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def height: Int = if (left.height < right.height)
    right.height + 1
  else
    left.height + 1

  def apply[B >: A](elem: B): Int = {
    if (head.key.equals(elem)) {
      return head.count
    }
    if (head.key.hashCode() >= elem.hashCode()) {
      left(elem)
    } else {
      right(elem)
    }
  }

  override def min: Node[A] =
    if (left.isEmpty)
      head
    else
      left.min

  override def max: Node[A] =
    if (right.isEmpty)
      head
    else
      right.max



  def add[B >: A](elem: B, count: Int): Tree[B] = {
    if (head.key.equals(elem)) {
      val newHead = new Node[B](elem, head.count + count)
      return Cons(newHead, left, right)
    }
    if (elem.hashCode() < head.key.hashCode()) {
      val newLeft = left.add(elem, count)
      return balanced(head, newLeft, right)
    }
    val newRight = right.add(elem, count)
    balanced(head, left, newRight)
  }

  def remove[B >: A](elem: B, count: Int): Tree[B] = {
    if (head.key.equals(elem)) {
      if (head.count > count) {
        val newHead = new Node[B](elem, head.count - count)
        return Cons(newHead, left, right)
      } else if (left == Nil && right == Nil) {
          return Nil
      } else if (left.height >= right.height) {
        val newHead = left.max
        val newLeft = left.remove(newHead.key, newHead.count)
        return balanced(newHead, newLeft, right)
      } else {
        val newHead = right.min
        val newRight = right.remove(newHead.key, newHead.count)
        return balanced(newHead, left, newRight)
      }

    }
    if (head.key.hashCode() >= elem.hashCode()) {
      val newLeft = left.remove(elem, count)
      balanced(head, newLeft, right)
    } else {
      val newRight = right.remove(elem, count)
      balanced(head, left, newRight)
    }
  }

  override def |[B >: A](other: Tree[B]): Tree[B] = {
    other match {
      case Cons(_, _, _) =>
        val headElemCount = apply(other.head.key)
        var withHead: Tree[B] = this
        if (other.head.count > headElemCount) {
          withHead = add(other.head.key, other.head.count - headElemCount)
        }
        withHead | other.left | other.right
      case Nil => this
    }
  }

  override def &[B >: A](other: Tree[B]): Tree[B] = {
    other match {
      case Cons(_, _, _) =>
        val withLeft = this & other.left
        val withRight = this & other.right
        val toReturn = withLeft | withRight
        val headElemCount = apply(other.head.key)
        if (headElemCount > 0) {
          if (headElemCount < other.head.count) {
            return toReturn.add(other.head.key, headElemCount)
          }
          return toReturn.add(other.head.key, other.head.count)
        }
        toReturn
      case Nil => Nil
    }
  }

  override def contains(elem: Any): Boolean = {
    if (head.key.equals(elem)) {
      return true
    }
    if (head.key.hashCode() >= elem.hashCode()) {
      return left.contains(elem)
    }
    right.contains(elem)
  }

  override def isEmpty: Boolean = false

  override def iterator: Iterator[Node[A]] = new Iterator[Node[A]] {
    private var branch = 0
    private val leftIter = left.iterator
    private val rightIter = right.iterator
    override def hasNext: Boolean = {
      branch match {
        case 0 => true
        case 1 => leftIter.hasNext || rightIter.hasNext
        case 2 => rightIter.hasNext
      }
    }

    @scala.annotation.tailrec
    override def next(): Node[A] = {
      branch match {
        case 0 =>
          branch = 1
          head
        case 1 =>
          if (leftIter.hasNext) {
            leftIter.next
          }else {
            branch = 2
            next()
          }
        case 2 => rightIter.next()
      }
    }
  }

  override def foreach(function: A => Unit): Unit = {
    for (i <- 0 to head.count) {
      function(head.key)
    }
    left.foreach(function)
    right.foreach(function)
  }

}

case object Nil extends Tree[Nothing] {
  override def head: Node[Nothing] = null

  override def left: Tree[Nothing] = throw new NoSuchElementException

  override def right: Tree[Nothing] = throw new NoSuchElementException

  override def height: Int = 0

  override def apply[B >: Nothing](elem: B): Int = 0

  override def add[B >: Nothing](elem: B, count: Int): Tree[B] =
    Cons(new Node[B](elem, count), Nil, Nil)

  override def |[B >: Nothing](other: Tree[B]): Tree[B] = other

  override def &[B >: Nothing](other: Tree[B]): Tree[B] = Nil

  override def contains(elem: Any): Boolean = false

  override def isEmpty: Boolean = true

  override def iterator: Iterator[Node[Nothing]] = new Iterator[Node[Nothing]] {
    override def hasNext: Boolean = false

    override def next(): Nothing = throw new NoSuchElementException
  }

  override def remove[B >: Nothing](elem: B, count: Int): Tree[B] = Nil

  override def min: Node[Nothing] = null

  override def max: Node[Nothing] = null

  override def foreach(function: Nothing => Unit): Unit = {}

}


case class MultiSet[A](elems: A*) {

  private var tree: Tree[A] = {
    var set: Tree[A] = Nil
    for (elem <- elems) {
      set = set + elem
    }
    set
  }

  def add(elem: A, count: Int): Unit = {
    tree = tree.add(elem, count)
  }

  def add (elem: A): Unit = {
    add(elem, 1)
  }

  def apply(elem: A): Int = tree(elem)

  def +(elem: A): Unit = {
    tree = tree + elem
  }

  def remove(elem: A, count: Int): Unit = {
    tree = tree.remove(elem, count)
  }

  def remove(elem: A): Unit = {
    remove(elem, 1)
  }

  def |[B >: A](other: MultiSet[B]): MultiSet[B] = {
    val result = MultiSet[B]()
    result.tree = tree | other.tree
    result
  }

  def &[B >: A](other: MultiSet[B]): MultiSet[B] = {
    val result = MultiSet[B]()
    result.tree = tree & other.tree
    result
  }

  def contains(elem: Any): Boolean = {
    tree.contains(elem)
  }

  def isEmpty: Boolean = tree.isEmpty

  def iterator: Iterator[A] = new Iterator[A] {
    private val treeIter = tree.iterator
    private var counter = 0
    var current: Node[A] = _

    override def hasNext: Boolean = {
      if (treeIter.hasNext) true
      else if (current != null) {
        counter < current.count
      } else false
    }

    override def next(): A = {
      if (current == null)
        current = treeIter.next()
      if (counter >= current.count) {
        current = treeIter.next()
        counter = 0
      }
      counter += 1
      current.key
    }
  }

  def foreach(function: A => Unit): Unit = {
    tree.foreach(function)
  }

  def map[B](function: A => B): MultiSet[B] = {
    val result = MultiSet[B]()
    for (node: Node[A] <- tree.iterator) {
      result.add(function(node.key), node.count)
    }
    result
  }

  override def toString: String = {
    val sb = new StringBuilder()
    sb.append("[")
    for (node: Node[A] <- tree.iterator) {
      sb.append(node.key)
      sb.append(" -> ")
      sb.append(node.count)
      sb.append(", ")
    }
    sb.delete(sb.length() - 2, sb.length())
    sb.append("]")
    sb.result()
  }

}