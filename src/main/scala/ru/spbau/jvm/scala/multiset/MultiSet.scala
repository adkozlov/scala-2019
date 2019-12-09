package ru.spbau.jvm.scala.multiset

class MultiSet[K](private var _size: Int = 0, private var tree: Tree[(K, Int)] = Leaf[(K, Int)]())(implicit ordering: Ordering[K]) extends Collection[K] {
  override def size(): Int = _size

  override def contains(key: K): Boolean = tree match {
    case Leaf() => false
    case node: Node[K] =>
      tree = node.find((key, 0)).splay()
      ordering.equiv(tree.key._1, key)
  }

  def iterator(): Iterator[(K, Int)] = new MultiSetIterator[K](tree.findMin())

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
    foreach(x => acc = f.apply(acc, x))
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

  override def add(key: K): Unit = {
    insert(key, 1)
  }

  private def insert(key: K, cnt: Int): (K, Int) = {
    val (left, right) = tree.split((key, 0))
    if (ordering.equiv(right.key._1, key)) {
      val (_, oldNum) = right.key
      right.key = (right.key._1, right.key._2 + 1)
      tree = left.merge(right)
      (key, oldNum + cnt)
    } else {
      tree = left.merge(new Node[(K, Int)]((key, 1)).merge(right))
      (key, cnt)
    }
  }

  override def remove(key: K): Unit = {
    delete(key)
  }

  private def delete(key: K): (K, Int) = {
    val (left, right) = tree.split((key, 0))
    if (right.key._1 == key) {
      if (right.key._2 == 1) {
        tree = left.merge(right.left.merge(right.right))
        (key, 1)
      } else {
        val (_, oldNum) = right.key
        right.key = (right.key._1, right.key._2 - 1)
        tree = left.merge(right)
        (key, oldNum)
      }
    } else {
      tree = left.merge(right)
      (key, 0)
    }
  }

  override def clear(): Unit = {
    tree = Leaf[(K, Int)]()
  }

  def |(other: MultiSet[K]): MultiSet[K] = {
    val result = MultiSet[K]()
    this.foreachPair(key => result.insert(key._1, key._2))
    other.foreachPair(key => result.insert(key._1, key._2))
    result
  }

  def &(other: MultiSet[K]): MultiSet[K] = {
    val result = MultiSet[K]()
    this.foreachPair(it => if (other.contains(it._1)) result.insert(it._1, it._2))
    result
  }

  override def toString(): String = {
    val list: List[String] = List[String]()
    foreachPair(it => list.appended(s"${it._1} -> ${it._2}"))
    list.mkString("[", ", ", "]")
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

class MultiSetIterator[K](private var tree: Tree[(K, Int)]) extends Iterator[(K, Int)] {

  var leftOnPath = 0
  var curKey: (K, Int) = tree.key

  override def hasNext: Boolean = {
    curKey._2 > 0 || leftOnPath > 0 || tree.right.size > 1
  }

  override def next(): (K, Int) = {
    if (curKey._2 == 0) {
      tree match {
        case Leaf() => tree = Leaf[(K, Int)]()
        case node: Node[(K, Int)] => node.right match {
          case Leaf() =>
            while (tree.isRightChild) {
              tree = tree.parent
            }
            leftOnPath -= 1
          case r: Node[(K, Int)] =>
            tree = r
            while (tree.left.size != 0) {
              tree = tree.left
              leftOnPath += 1
            }
        }
      }
      curKey = tree.key
    }
    curKey = (curKey._1, curKey._2 - 1)
    curKey
  }
}