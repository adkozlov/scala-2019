package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.AvlBalance.AvlBalanceType

object AvlRotation extends Enumeration {
  type AvlRotationType = Value

  val AvlRotSmallLeft, AvlRotSmallRight, AvlRotBigLeft, AvlRotBigRight, AvlRotNone = Value
}

object AvlBalance extends Enumeration {
  type AvlBalanceType = Value
  val AvlBalanced, AvlRightBalanced, AvlLeftBalanced, AvlRightUnbalanced, AvlLeftUnbalanced = Value

  def isBalanced(balance: AvlBalanceType): Boolean = balance match {
    case AvlBalanced => true
    case AvlLeftBalanced => true
    case AvlRightBalanced => true
    case _ => false
  }

  def isLeftOrBalanced(balance: AvlBalanceType): Boolean = balance match {
    case AvlBalanced => true
    case AvlLeftBalanced => true
    case AvlLeftUnbalanced => true
    case _ => false
  }

  def isRightOrBalanced(balance: AvlBalanceType): Boolean = balance match {
    case AvlBalanced => true
    case AvlRightBalanced => true
    case AvlRightUnbalanced => true
    case _ => false
  }
}

sealed trait AvlTree[T] {
  def count: Int

  def depth: Int

  def isEmpty: Boolean

  def notEmpty: Boolean

  def apply(elem: T): Int

  def add(data: T): AvlTree[T]

  def add(data: T, count: Int): AvlTree[T]

  def remove(data: T): AvlTree[T]

  def contains(elem: T): Boolean

  def rebalance(): AvlTree[T]

  def balance: AvlBalanceType

  def min: Option[(T, Int)]

  def max: Option[(T, Int)]

  def foreach(f: T => Unit): Unit
}

final case class AvlNode[T](depth: Int,
                            count: Int,
                            data: T,
                            private val left: AvlTree[T],
                            private val right: AvlTree[T])
                           (implicit val ordering: Ordering[T]) extends AvlTree[T] {

  override def apply(elem: T): Int = {
    if (ordering.equiv(elem, data)) {
      count
    } else if (ordering.lt(elem, data)) {
      left(elem)
    } else {
      right(elem)
    }
  }

  override def add(value: T): AvlTree[T] = add(value, 1)

  override def add(value: T, addCount: Int): AvlTree[T] =
    if (ordering.equiv(value, data)) {
      updateTree(addCount)
    } else if (ordering.lt(value, data)) {
      makeTree(data, count, left.add(value, addCount), right).rebalance()
    } else {
      makeTree(data, count, left, right.add(value, addCount)).rebalance()
    }

  override def rebalance(): AvlTree[T] = {
    balance match {
      case factor if AvlBalance.isBalanced(factor) => this
      //  Left rotation
      case AvlBalance.AvlRightUnbalanced =>
        right.balance match {
          // Small left rotation
          case factor if AvlBalance.isRightOrBalanced(factor) => rotateLeft()
          // Big left rotation
          case _ =>
            val rightNode = right.asInstanceOf[AvlNode[T]]
            val newRight = rightNode.rotateRight()
            makeTree(data, count, left, newRight).rotateLeft()
        }
      // Right rotation
      case AvlBalance.AvlLeftUnbalanced =>
        left.balance match {
          // Small right rotation
          case factor if AvlBalance.isLeftOrBalanced(factor) => rotateRight()
          // Big right rotation
          case _ =>
            val leftNode = left.asInstanceOf[AvlNode[T]]
            val newLeft = leftNode.rotateLeft()
            makeTree(data, count, newLeft, right).rotateRight()
        }
    }
  }

  override def balance: AvlBalanceType = right.depth - left.depth match {
    case 0 => AvlBalance.AvlBalanced
    case -1 => AvlBalance.AvlLeftBalanced
    case -2 => AvlBalance.AvlLeftUnbalanced
    case 1 => AvlBalance.AvlRightBalanced
    case 2 => AvlBalance.AvlRightUnbalanced
    case factor => throw new IllegalStateException(s"Broken AVL invariant! Factor: $factor")
  }

  private def rotateLeft(): AvlTree[T] = {
    if (!right.isInstanceOf[AvlNode[T]]) {
      this
    } else {
      val rightNode = right.asInstanceOf[AvlNode[T]]
      val newLeftNode = makeTree(data, count, left, rightNode.left)
      makeTree(rightNode.data, rightNode.count, newLeftNode, rightNode.right)
    }
  }

  private def makeTree[U](data: U, count: Int, left: AvlTree[U], right: AvlTree[U])(implicit o: Ordering[U]): AvlNode[U]
  = AvlNode(1 + math.max(left.depth, right.depth), count, data, left, right)

  private def rotateRight(): AvlTree[T] = {
    if (!left.isInstanceOf[AvlNode[T]]) {
      this
    } else {
      val leftNode = left.asInstanceOf[AvlNode[T]]
      val newRightNode = makeTree(data, count, leftNode.left, right)
      makeTree(leftNode.data, leftNode.count, leftNode.left, newRightNode)
    }
  }

  private def updateTree(diff: Int): AvlNode[T]
  = AvlNode(depth, count + diff, data, left, right)

  override def remove(value: T): AvlTree[T] =
    if (ordering.equiv(value, data)) {
      if (count > 1) {
        updateTree(-1)
      } else if (left.isInstanceOf[AvlNil[T]]) {
        right
      } else if (right.isInstanceOf[AvlNil[T]]) {
        left
      } else {
        // We know that `left` isn't AvlNil so `get` won't fail
        val (newRootData, newRootDataCount) = left.max.get
        val newLeft = left.asInstanceOf[AvlNode[T]].removeMostRight()
        makeTree(newRootData, newRootDataCount, newLeft, right).rebalance()
      }
    } else if (ordering.lt(data, value)) {
      makeTree(data, count, left, right.remove(value))
    } else {
      makeTree(data, count, left.remove(value), right)
    }

  override def min: Option[(T, Int)] = if (left.isInstanceOf[AvlNil[T]]) Some(data, count) else left.min

  override def max: Option[(T, Int)] = if (right.isInstanceOf[AvlNil[T]]) Some(data, count) else right.max

  override def contains(elem: T): Boolean = {
    if (ordering.equiv(elem, data)) {
      true
    } else if (ordering.lt(elem, data)) {
      left.contains(elem)
    } else {
      right.contains(elem)
    }
  }

  override def toString: String = {
    val leftString = left.toString
    val rightString = right.toString
    val headString = if (leftString.isEmpty) "" else s"$leftString, "
    val tailString = if (rightString.isEmpty) "" else s", $rightString"
    s"$headString$data -> $count$tailString"
  }

  override def isEmpty: Boolean = false

  override def notEmpty: Boolean = true

  override def foreach(f: T => Unit): Unit = {
    left.foreach(f)
    // There's for(i <- 0 until count) construction but it uses scala.collection.immutable.Range
    // internally which may be forbidden.
    var counter = count
    while (counter > 0) {
      f(data)
      counter -= 1
    }
    right.foreach(f)
  }

  private def removeMostRight(): AvlTree[T] = if (right.isInstanceOf[AvlNil[T]]) {
    left
  } else {
    makeTree(data, count, left, right.asInstanceOf[AvlNode[T]].removeMostRight()).rebalance()
  }
}

final case class AvlNil[T]()(implicit val ordering: Ordering[T]) extends AvlTree[T] {
  override def apply(elem: T): Int = 0

  override def add(data: T): AvlTree[T] = AvlNode(1, 1, data, AvlNil(), AvlNil())

  override def add(data: T, count: Int): AvlTree[T] = AvlNode(1, count, data, AvlNil(), AvlNil())

  override def remove(data: T): AvlTree[T] = throw new UnsupportedOperationException

  override def rebalance(): AvlTree[T] = AvlNil()

  override def depth: Int = 0

  override def count: Int = 0

  override def balance: AvlBalanceType = AvlBalance.AvlBalanced

  override def min: Option[(T, Int)] = None

  override def max: Option[(T, Int)] = None

  override def contains(elem: T): Boolean = false

  override def toString: String = ""

  override def isEmpty: Boolean = true

  override def notEmpty: Boolean = false

  override def foreach(f: T => Unit): Unit = {}
}
