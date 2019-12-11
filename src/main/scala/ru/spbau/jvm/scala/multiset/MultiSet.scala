package ru.spbau.jvm.scala.multiset

import scala.math.min

class MultiSet[K](private var tree: Tree[(K, Int)] = Leaf[(K, Int)]())(implicit ordering: Ordering[K]) extends Collection[K] {
  override def size(): Int = tree.size

  override def contains(key: K): Boolean = tree match {
    case Leaf() => false
    case node: Node[(K, Int)] =>
      tree = node.find((key, 0)).splay()
      tree match {
        case node: Node[(K, Int)] => ordering.equiv(node.key._1, key)
        case _ => false
      }
  }

  def count(key: K): Int = {
    if (!contains(key)) 0
    else {
      tree match {
        case Leaf() => 0
        case node: Node[(K, Int)] => node.key._2
      }
    }
  }
  def iterator(): Iterator[(K, Int)] = tree match {
    case Leaf() => new MultiSetIterator[K](Leaf(), 0)
    case node: Node[(K, Int)] =>  new MultiSetIterator[K](node.findMin(), node.size)
  }

  override def add(key: K): Unit = {
    insert(key, 1)
  }

  private def insert(key: K, cnt: Int): (K, Int) = {
    val (left, right) = tree.split((key, 0))
    right match {
      case right: Node[(K, Int)] =>
        if (ordering.equiv(right.key._1, key)) {
          val (_, oldNum) = right.key
          right.key = (right.key._1, right.key._2 + cnt)
          tree = left.merge(right)
          return (key, oldNum + cnt)
        }
      case _ =>
    }
    tree = left.merge(new Node[(K, Int)]((key, cnt), Leaf(), Leaf()).merge(right))
    (key, cnt)
  }

  override def remove(key: K): Unit = {
    delete(key)
  }

  private def delete(key: K): (K, Int) = {
    val (left, right) = tree.split((key, 0))
    right match {
      case right: Node[(K, Int)] =>
      if (right.key._1 == key) {
        if (right.key._2 == 1) {
          tree = left.merge(right.left.merge(right.right))
          return (key, 1)
        } else {
          val (_, oldNum) = right.key
          right.key = (right.key._1, right.key._2 - 1)
          tree = left.merge(right)
          return (key, oldNum)
        }
      }
    }
    tree = left.merge(right)
    (key, 0)
  }

  override def clear(): Unit = {
    tree = Leaf()
  }

  def foreach(f: K => Unit): Unit = {
    val it = iterator()
    while (it.hasNext) {
      f.apply(it.next()._1)
    }
  }

  def foreachPair(f: ((K, Int)) => Unit): Unit = {
    val it = iterator()
    while (it.hasNext) {
      f.apply(it.next())
    }
  }

  def foldLeft[T](z: T)(f: (T, K) => T): T = {
    var acc = z
    foreach(it => acc = f.apply(acc, it))
    acc
  }

  def foldRight[T](z: T)(f: (K, T) => T): T = {
    def foldr(it: Iterator[(K, Int)]) : T = {
      if (it.hasNext) {
        val cur = it.next()
        f.apply(cur._1, foldr(it))
      } else {
        z
      }
    }
    foldr(iterator())
  }

  def |(other: MultiSet[K]): MultiSet[K] = {
    val result = MultiSet[K]()
    this.foreachPair(it => result.insert(it._1, it._2))
    other.foreachPair(it => result.insert(it._1, it._2))
    result
  }

  def &(other: MultiSet[K]): MultiSet[K] = {
    val result = MultiSet[K]()
    this.foreachPair(it => if (other.contains(it._1)) result.insert(it._1, min(it._2, other.count(it._1))))
    result
  }

  override def toString: String = {
    var result: String = "["
    foreachPair(it => result += s"${it._1} -> ${it._2}, ")
    if (result.length > 1) {
      result = result.substring(0, result.length - 2)
    }
    result + "]"
  }
}

object MultiSet {
  def apply[K]()(implicit ord: Ordering[K]): MultiSet[K] = new MultiSet[K]()

  def apply[K](args : K*)(implicit ord: Ordering[K]): MultiSet[K] = {
    val multiset = new MultiSet[K]()
    args.foreach(multiset.add)
    multiset
  }
}

class MultiSetIterator[K](private var tree: Tree[(K, Int)], private var size: Int) extends Iterator[(K, Int)] {

  var nextKey: (K, Int) = tree match {
    case Leaf() => null
    case node: Node[(K, Int)] => node.key
  }
  var ind: Int = 0

  override def hasNext: Boolean = {
    ind < size
  }

  override def next(): (K, Int) = {
    val curKey = nextKey
    if (ind < size - 1) {
      tree match {
        case _node: Node[(K, Int)] => _node.right match {
          case Leaf() =>
            var node = _node
            while (node.isRightChild) {
              node.parent match {
                case parent: Node[(K, Int)] =>
                  node = parent
              }
            }
            node.parent match {
              case parent: Node[(K, Int)] =>
                node = parent
            }
            tree = node
            nextKey = node.key
          case right: Node[(K, Int)] =>
            val node = right.findMin()
            tree = node
            nextKey = node.key
        }
      }
    }
    ind += 1
    curKey
  }
}