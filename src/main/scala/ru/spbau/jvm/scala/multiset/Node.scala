package ru.spbau.jvm.scala.multiset

import scala.math.max

class Node[A](var value: A) {
  private var height = 1
  var left: Option[Node[A]] = None
  var right: Option[Node[A]] = None
  var parent: Option[Node[A]] = None

  private def diff(): Int = Node.height(left) - Node.height(right)

  private def rotate(isRight: Boolean): Node[A] = {
    val tmpNode = if (isRight) left else right
    val tmpSon = if (isRight) tmpNode.get.right else tmpNode.get.left
    Node.changeSon(parent, tmpNode, this)
    Node.setSon(Some(this), tmpSon, !isRight)
    Node.setSon(tmpNode, Some(this), isRight)
    Node.update(this)
    Node.update(tmpNode)
    tmpNode.get
  }

  private def rotateRight(): Node[A] = rotate(true)

  private def rotateLeft(): Node[A] = rotate(false)

  private def bigRotateRight(): Node[A] = {
    left = Some(left.get.rotateLeft())
    rotateRight()
  }

  private def bigRotateLeft(): Node[A] = {
    right = Some(right.get.rotateRight())
    rotateLeft()
  }

  def balance(): Node[A] = {
    Node.update(this)
    val balance = diff()
    if (balance > 1) {
      if (left.get.diff() > 0)
        return rotateRight()
      else
        return bigRotateRight()
    }
    if (balance < -1) {
      if (right.get.diff() > 0)
        return bigRotateLeft()
      else
        return rotateLeft()
    }
    this
  }

  def cascadingBalance(): Node[A] = {
    var tmp: Option[Node[A]] = Some(this)
    while (tmp.get.parent.isDefined) {
      tmp = tmp.get.balance().parent
    }
    tmp.get.balance()
  }
}

private object Node {

  private def setParent[A](parent: Option[Node[A]],
                           son: Option[Node[A]]): Unit = {
    son match {
      case Some(node) => node.parent = parent
      case None =>
    }
  }

  def setSon[A](parent: Option[Node[A]],
                son: Option[Node[A]],
                isRight: Boolean): Unit = {
    parent match {
      case Some(node) =>
        if (isRight)
          node.right = son
        else
          node.left = son
      case None =>
    }
    setParent(parent, son)
  }

  def changeSon[A](parent: Option[Node[A]],
                   son: Option[Node[A]],
                   oldSon: Node[A]): Unit = {
    parent match {
      case Some(node) =>
        val isRight = node.right.isDefined && node.right.get.eq(oldSon)
        Node.setSon(parent, son, isRight)
      case None => setParent(parent, son)
    }
  }

  private def height[A](node: Option[Node[A]]): Int = node match {
    case Some(node) => node.height
    case None => 0
  }

  private def update[A](node: Option[Node[A]]): Unit = node match {
    case Some(node) => update(node)
    case None =>
  }

  private def update[A](node: Node[A]): Unit = {
    node.height = 1 + max(height(node.left), height(node.right))
  }

  def nextNode[A](node: Node[A]): Option[Node[A]] = {
    node.right match {
      case Some(node) =>
        var currentNode = node
        while (currentNode.left.isDefined) {
          currentNode = currentNode.left.get
        }
        Some(currentNode)
      case None =>
        var currentNode = node
        while (isRightSon(currentNode)) {
          currentNode = currentNode.parent.get
        }
        currentNode.parent
    }
  }

  private def isRightSon[A](node: Node[A]): Boolean = {
    val hasParent = node.parent.isDefined
    val parentHasRightSon = hasParent && node.parent.get.right.isDefined
    val isRightSon = parentHasRightSon && node.parent.get.right.get.eq(node)
    hasParent && parentHasRightSon && isRightSon
  }

  def deleteNode[A](delNode: Node[A]): Option[Node[A]] = {
    var node: Node[A] = delNode
    if (node.left.isDefined && node.right.isDefined) {
      val next = Node.nextNode(node).get
      node.value = next.value
      node = next
    }

    val son = if (node.left.isDefined) node.left else node.right
    Node.changeSon(node.parent, son, node)
    node.parent match {
      case Some(node) => Some(node.cascadingBalance())
      case None => son
    }
  }
}
