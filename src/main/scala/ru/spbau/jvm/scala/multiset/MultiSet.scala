package ru.spbau.jvm.scala.multiset

class MultiSet[K](elements: K*)(implicit ord: Ordering[K]) {
  private var tree = new AVLTreeMultiSet[K]
  private var _size = 0

  elements.foreach {e => add(e)}

  def add(key: K): Unit = {
    tree.add(key)
    _size += 1
  }

  def apply(key: K): Int = count(key)

  def clear(): Unit = {
    tree = new AVLTreeMultiSet[K]
    _size = 0
  }

  def contains(key: K): Boolean = count(key) > 0

  def count(key: K): Int = tree.find(key)

  def filter(f: K => Boolean): MultiSet[K] = {
    val result = new MultiSet[K]()
    foreach {element => if (f(element)) result.add(element)}
    result
  }

  def flatMap[L](fun: K => MultiSet[L])(implicit ord: Ordering[L]): MultiSet[L] = {
    val result = new MultiSet[L]()
    foreach(element => fun(element).foreach(e => result.add(e)))
    result
  }

  def foreach(fun: K => Unit): Unit = tree.foreach(fun)

  def iterator(): Iterator[K] = new Iterator[K] {
    private val iter = tree.iterator()
    private var cur: (K, Int) = _

    override def hasNext: Boolean = (cur != null && cur._2 > 0) || iter.hasNext

    override def next(): K = {
      if (cur == null || cur._2 == 0)
        cur = iter.next()
      cur = (cur._1, cur._2 - 1)
      cur._1
    }
  }

  def map[L](fun: K => L)(implicit ord: Ordering[L]): MultiSet[L] = {
    val result = new MultiSet[L]()
    foreach {element => result.add(fun(element))}
    result
  }

  def remove(key: K): Unit = {
    if (contains(key))
      _size -= 1
    tree.remove(key)
  }

  def removeAll(key: K): Unit = {
    _size -= count(key)
    tree.remove(key, all = true)
  }

  def size(): Int = _size

  def &(other: MultiSet[K]): MultiSet[K] = {
    filter(element => other.contains(element)) |
      other.filter(element => contains(element))
  }

  def |(other: MultiSet[K]): MultiSet[K] = {
    val result = new MultiSet[K]()
    foreach {element => result.add(element)}
    other.foreach {element => result.add(element)}
    result
  }

  override def toString: String = {
    var result: String = "["
    val iter = tree.iterator()
    while (iter.hasNext) {
      val node = iter.next()
      result += s"${node._1} -> ${node._2}"
      if (iter.hasNext)
        result += ", "
    }
    result += "]"
    println(result)
    result
  }
}

private class AVLTreeMultiSet[K]()(implicit ord: Ordering[K]){
  import ord._

  class AVLTreeNode(var key: K,
                    var value: Int = 1,
                    var height: Int = 0,
                    var left: AVLTreeNode = null,
                    var right: AVLTreeNode = null) {
    def computeHeight(): Unit = {
      height = math.max(getHeight(left), getHeight(right)) + 1
    }
  }

  class BSTIterator  {
    private var curNode: AVLTreeNode = _
    private var lastNode: (K, Int) = _

    def hasNext: Boolean = {
      if (lastNode == null) {
        if (curNode == null)
          curNode = findLeft(root)
        return curNode != null
      }
      if (curNode != null) return true
      curNode = findNext(root, lastNode._1)
      curNode != null
    }

    def next(): (K, Int) = {
      if (!hasNext)
        throw new NoSuchElementException("Iterator came out of bounds")
      lastNode = (curNode.key, curNode.value)
      curNode = null
      lastNode
    }
  }

  private def getHeight(node: AVLTreeNode): Int = {
    if (node == null) -1
    else node.height
  }

  var root: AVLTreeNode = _

  def iterator(node: AVLTreeNode = root): BSTIterator = {
    new BSTIterator()
  }

  def foreach(fun: K => Unit, node: AVLTreeNode = root): Unit = {
    if (node == null) return
    foreach(fun, node.left)
    for (_ <- 0 until node.value) {
      fun(node.key)
    }
    foreach(fun, node.right)
  }

  def find(element: K, node: AVLTreeNode = root): Int = {
    if (node == null) return 0
    if (element < node.key) return find(element, node.left)
    if (element > node.key) return find(element, node.right)
    node.value
  }

  def add(element: K): Unit = {
    root = addImpl(element, root)
  }

  private def addImpl(element: K, node: AVLTreeNode): AVLTreeNode = {
    if (node == null) return new AVLTreeNode(element)

    if (node.key.equals(element)) {
      node.value += 1
      return node
    }

    if (element < node.key) {
      node.left = addImpl(element, node.left)
      balanceLeft(node)
    } else {
      node.right = addImpl(element, node.right)
      balanceRight(node)
    }
  }

  def remove(element: K, all: Boolean = false): Unit = {
    root = removeImpl(element, root, all)
  }

  private def removeImpl(element: K, node: AVLTreeNode, all: Boolean): AVLTreeNode = {
    if (node == null) return node

    if (node.key.equals(element)) {
      if (!all && node.value > 1) {
        node.value -= 1
        return node
      }
      node.value = 1

      if (node.left == null && node.right == null) return null

      if (node.right == null) return node.left

      if (node.left == null) return node.right

      if (getHeight(node.left) > getHeight(node.right)) {
        val closestNode = findRight(node.left)
        node.key = closestNode.key
        node.value = closestNode.value
        closestNode.value = 1
        node.left = removeImpl(node.key, node.left, all)
        return node
      } else {
        val closestNode = findLeft(node.right)
        node.key = closestNode.key
        node.value = closestNode.value
        closestNode.value = 1
        node.right = removeImpl(node.key, node.right, all)
        return node
      }
    }

    if (element < node.key) {
      node.left = removeImpl(element, node.left, all)
      balanceRight(node)
    } else {
      node.right = removeImpl(element, node.right, all)
      balanceLeft(node)
    }
  }

  @scala.annotation.tailrec
  private def findRight(node: AVLTreeNode): AVLTreeNode = {
    if (node.right == null) node
    else findRight(node.right)
  }

  @scala.annotation.tailrec
  private def findLeft(node: AVLTreeNode): AVLTreeNode = {
    if (node.left == null) node
    else findLeft(node.left)
  }

  private def findNext(node: AVLTreeNode, element: K): AVLTreeNode = {
    if (node == null) return null
    if (node.key <= element) return findNext(node.right, element)
    val leftNext = findNext(node.left, element)
    if (leftNext == null) node
    else leftNext
  }

  private def balanceLeft(node: AVLTreeNode): AVLTreeNode = {
    if (getHeight(node.left) - getHeight(node.right) > 1) {
      if (getHeight(node.left.left) >= getHeight(node.left.right))
        return singleRightRotate(node)
      else
        return doubleRightRotate(node)
    }
    node.computeHeight()
    node
  }

  private def balanceRight(node: AVLTreeNode): AVLTreeNode = {
    if (getHeight(node.right) - getHeight(node.left) > 1) {
      if (getHeight(node.right.right) >= getHeight(node.right.left))
        return singleLeftRotate(node)
      else {
        return doubleLeftRotate(node)
      }
    }
    node.computeHeight()
    node
  }

  private def singleLeftRotate(node: AVLTreeNode): AVLTreeNode = {
    val rightSon = node.right
    node.right = rightSon.left
    rightSon.left = node
    node.computeHeight()
    rightSon.computeHeight()
    rightSon
  }

  private def singleRightRotate(node: AVLTreeNode): AVLTreeNode = {
    val leftSon = node.left
    node.left = leftSon.right
    leftSon.right = node
    node.computeHeight()
    leftSon.computeHeight()
    leftSon
  }

  private def doubleLeftRotate(node: AVLTreeNode): AVLTreeNode = {
    node.right = singleRightRotate(node.right)
    singleLeftRotate(node)
  }

  private def doubleRightRotate(node: AVLTreeNode): AVLTreeNode = {
    node.left = singleLeftRotate(node.left)
    singleRightRotate(node)
  }
}