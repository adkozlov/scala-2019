package ru.spbau.jvm.scala.multiset

abstract sealed class Tree[K] {

  var size: Int

  // < x / >= x
  def split(key: K)(implicit ordering: Ordering[K]): (Tree[K], Tree[K]) = this match {
    case Leaf() => (Leaf(), Leaf())
    case tree: Node[K] =>
      val node = tree.find(key).splay()
      if (ordering.lt(node.key, key)) {
        return (node, Leaf())
      }
      val left = node.left
      node.left = Leaf()
      node.recalc()
      left match {
        case Leaf() =>
        case _left: Node[K] => _left.parent = Leaf()
      }
      (left, node)

  }

  def merge(rightTree: Tree[K]): Tree[K] = {
    (this, rightTree) match {
      case (Leaf(), Leaf()) => Leaf()
      case (Leaf(), _) => rightTree
      case (_, Leaf()) => this
      case (l: Node[K], r: Node[K]) =>
        val node = r.findMin().splay()
        node.left = l
        l.parent = node
        node.recalc()
        node
    }
  }

   def printTree(ident: String = ""): Unit = this match {
    case Leaf() =>
    case Node(key, left, right) =>
      right.printTree(ident + "  ")
      print(ident)
      println(key)
      left.printTree(ident + "  ")
  }

}

case class Node[K](var key: K, var left: Tree[K], var right: Tree[K])(implicit ordering: Ordering[K]) extends Tree[K]() {
  override var size: Int = 1 + left.size + right.size
  left match {
    case Leaf() =>
    case left: Node[K] => left.parent = this
  }
  right match {
    case Leaf() =>
    case right: Node[K] => right.parent = this
  }
  var parent: Tree[K] = Leaf()

  def recalc(): Unit = {
    this.size = left.size + right.size + 1
  }

  def isLeftChild: Boolean = this.parent match {
    case Leaf() => false
    case p: Node[K] => p.left == this
  }

  def isRightChild: Boolean = this.parent match {
    case Leaf() => false
    case p: Node[K] => p.right == this
  }

  def findMin(): Node[K] = this.left match {
    case Leaf() => this
    case left: Node[K] => left.findMin()
  }

  // min x: x >= k (lower_bound)
  def find(_key: K)(implicit ordering: Ordering[K]): Node[K] = {
    var tmp: Node[K] = this
    var node: Tree[K] = this
    while (node.size > 0)
      node match {
        case _node: Node[K] =>
          if (ordering.lteq(_key, _node.key)) {
            tmp = _node
            node = _node.left
          } else {
            node = _node.right
          }
        case _ =>
      }
    tmp
  }

  def splay(): Node[K] = {
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
              rotateRight(grandpa)
              node.splay()
            } else {
              rotateRight(parent)
              rotateLeft(grandpa)
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
      case _ =>
    }
    node.right match {
      case right: Node[K] =>
        right.parent = node.parent
        val alpha = right.left
        right.left = node
        node.parent = right
        node.right = alpha
        alpha match {
          case alpha: Node[K] => alpha.parent = node
          case _ =>
        }
        node.recalc()
        right.recalc()
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
          parent.left = node.left
        } else {
          parent.right = node.left
        }
      case _ =>
    }
    node.left match {
      case left: Node[K] =>
        left.parent = node.parent
        val beta = left.right
        left.right = node
        node.parent = left
        node.left = beta
        beta match {
          case beta: Node[K] => beta.parent = node
          case _ =>
        }
        node.recalc()
        left.recalc()
    }
  }
}

case class Leaf[K]() extends Tree[K]() {
  override var size: Int = 0
}

object Leaf {
  def apply[K](): Leaf[K] = new Leaf()
}
