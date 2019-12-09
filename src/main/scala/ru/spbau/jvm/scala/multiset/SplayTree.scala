package ru.spbau.jvm.scala.multiset

abstract sealed class Tree[K](implicit ordering: Ordering[K]) {

  var size: Int
  var parent: Tree[K]
  var left: Tree[K]
  var right: Tree[K]
  var key: K

  private def recalc(tree: Tree[K]): Unit = tree match {
    case Node(_, left, right) => tree.size = left.size + right.size + 1
  }

  // <= x / > x
  def split(key: K): (Tree[K], Tree[K]) = this match {
    case _: Leaf[K] => (new Leaf[K], new Leaf[K])
    case tree: Node[K] =>
      val node = tree.find(key).splay()
      if (ordering.gt(key, node.key)) {
        val left = node.left
        node.left = new Leaf
        recalc(node)
        (left, node)
      } else {
        val right = node.right
        node.right = new Leaf
        recalc(node)
        (node, right)
      }
  }

  def merge(rightTree: Tree[K]): Tree[K] = {
    (this, rightTree) match {
      case (Leaf(), Leaf()) => new Leaf
      case (Leaf(), _) => rightTree
      case (_, Leaf()) => this
      case (l: Node[K], r: Node[K]) =>
        val node = l.findMin().splay()
        node.right = r
        r.parent = node
        recalc(node)
        node
    }
  }


  def findMin(): Tree[K] = this.left match {
    case Leaf() => this
    case left: Node[K] => left.findMin()
  }


  // min x: x >= k
  def find(_key: K): Tree[K] = {
    if (ordering.lt(_key, this.key)) {
      this.left match {
        case Leaf() => this
        case left: Node[K] => left.find(_key)
      }
    } else if (ordering.equiv(_key, this.key)) {
      this
    } else {
      this.right match {
        case Leaf() => this
        case right: Node[K] => right.find(_key)
      }
    }
  }

  def splay(): Tree[K] = {
    val node = this
    node.parent match {
      case Leaf() => node
      case parent: Node[K] =>
        parent.parent match {
          // zig
          case Leaf() =>
            if (node.isLeftChild) {
              rotateRight(parent)
              node
            } else {
              rotateLeft(parent)
              node
            }
          case grandpa: Node[K] =>
            // zig-zig
            if (node.isLeftChild && parent.isLeftChild) {
              rotateRight(grandpa)
              rotateRight(parent)
              node.splay()
            } else if (node.isRightChild && parent.isRightChild) {
              rotateLeft(grandpa)
              rotateLeft(parent)
              node.splay()
              // zig-zag
            } else if (node.isRightChild) {
              rotateLeft(parent)
              rotateRight(parent)
              node.splay()
            } else {
              rotateRight(parent)
              rotateLeft(parent)
              node.splay()
            }
        }
    }
  }

  //     node                  right
  //    / \                    / \
  //   /   \                node  \
  //  left  \       -->     / \    \
  //       right           /   \    beta
  //        / \          left   \
  //     alpha \               alpha
  //           beta
  private def rotateLeft(node: Node[K]): Unit = {
    node.parent match {
      case parent: Node[K] =>
        if (node.isLeftChild) {
          parent.left = node.right
        } else {
          parent.right = node.right
        }
    }
    node.right match {
      case right: Node[K] =>
        right.parent = node.parent
        val alpha = right.left
        right.left = node
        node.parent = right
        node.right = alpha
        alpha.parent = node
        recalc(node)
        recalc(right)
    }
  }

  //        node                  left
  //       / \                    / \
  //      /   \                  /   \
  //    left   \       -->     alpha  \
  //    / \    right                  node
  //   /   \                          / \
  // alpha  \                        /   \
  //       beta                    beta   \
  //                                    right
  private def rotateRight(node: Node[K]): Unit = {
    node.parent match {
      case parent: Node[K] =>
        if (node.isLeftChild) {
          parent.left = node.right
        } else {
          parent.right = node.right
        }
    }
    node.left match {
      case left: Node[K] =>
        left.parent = node.parent
        val beta = left.right
        left.right = node
        node.parent = left
        node.left = beta
        beta.parent = node
        recalc(node)
        recalc(left)
    }
  }

  def isLeftChild: Boolean = this.parent match {
    case _: Leaf[K] => false
    case p: Node[K] => p.left == this
  }

  def isRightChild: Boolean = this.parent match {
    case _: Leaf[K] => false
    case p: Node[K] => p.right == this
  }
}

case class Node[K](var key: K, var left: Tree[K] = Leaf[K](), var right: Tree[K] = Leaf[K]())(implicit ordering: Ordering[K]) extends Tree[K] {
      override var size: Int = 1
      var parent: Tree[K] = _
}

case class Leaf[K] extends Tree[K] {
  override var size: Int = 0
  override var parent: Tree[K] = _
  override var left: Tree[K] = _
  override var right: Tree[K] = _
  override var key: K = _
}
