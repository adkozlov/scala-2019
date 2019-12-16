package ru.spbau.jvm.scala.avl


import scala.util.Random

class MultiSet[T]()(implicit ord: T => Ordered[T]) {
  private var myHead: Node[T] = _
  private var mySize: Int = 0

  //Implement Treap, as one of the simplest
  case class Node[E](key: E,
                     var total: Int = 1, balancingKey: Int = Random.nextInt(),
                     //Tried option here, but it looked even worse
                     var left: Node[E] = null, var right: Node[E] = null)(implicit ord: E => Ordered[E]) {
    def get(key: E): Node[E] = {
      if (this.key == key) return this

      if (this.key < key) {
        if (right != null) {
          right.get(key)
        } else {
          null
        }
      } else {
        if (left != null) {
          left.get(key)
        } else {
          null
        }
      }
    }


    def add(key: E): Node[E] = {
      if (this.key == key) {
        total += 1
        return this
      }

      if (key < this.key) {
        left = if (left == null) {
          new Node[E](key)
        } else {
          left.add(key)
        }

        if (left.balancingKey > balancingKey) return rightPivot()
      } else if (key > this.key) {
        right = if (right == null) {
          new Node[E](key)
        } else {
          right.add(key)
        }

        if (right.balancingKey > balancingKey) return leftPivot()
      }

      this
    }

    def remove(key: E): Node[E] = {
      if (key == this.key) {
        if (total > 1) {
          total -= 1
        } else {
          if (left == null) return right
          if (right == null) return left

          if (left.balancingKey < right.balancingKey) {
            val removeFrom = leftPivot()
            removeFrom.left = removeFrom.left.remove(key)
            return removeFrom
          } else {
            val removeFrom = rightPivot()
            removeFrom.right = removeFrom.right.remove(key)
            return removeFrom
          }
        }
      }

      if (left != null && key < this.key) {
        left = left.remove(key)
      } else if (right != null && key > this.key) {
        right = right.remove(key)
      }

      this
    }

    private def rightPivot(): Node[E] = {
      val w = this.left
      val u = w.right

      this.left = u

      w.right = this

      w
    }

    private def leftPivot(): Node[E] = {
      val w = this.right
      val u = w.left

      this.right = u

      w.left = this

      w
    }

    def toList: List[(E, Int)] = {
      if (this.left != null && this.right != null) {
        this.left.toList ::: ((this.key, this.total) :: this.right.toList)
      } else if (this.left != null) {
        this.left.toList ::: ((this.key, this.total) :: Nil)
      } else if (this.right != null) {
        (this.key, this.total):: this.right.toList
      } else {
        (this.key, this.total) :: Nil
      }
    }
  }

  def add(e: T)(implicit ord: T => Ordered[T]): MultiSet[T] = {
    myHead = if (myHead == null) {
      Node(e)
    } else {
      myHead.add(e)
    }

    mySize += 1
    this
  }

  def contains(key: T): Boolean = if (myHead != null) {
    myHead.get(key) != null
  } else {
    false
  }


  def remove(o: T): MultiSet[T] = {
    if (myHead != null && contains(o)) {
      myHead = myHead.remove(o)
      mySize -= 1
    }
    this
  }

  def map[U](function: T => U)(implicit ord: U => Ordered[U]): MultiSet[U] = MultiSet(toList.map(function))

  def filter(predicate: T => Boolean)(implicit ord: T => Ordered[T]): MultiSet[T] = MultiSet(toList.filter(predicate))

  def withFilter(predicate: T => Boolean): MultiSet[T] = filter(predicate)

  def foreach(function: T => Unit): Unit = toList.foreach(function)

  //Sum by whole sum, not minimum
  def |(other: MultiSet[T]): MultiSet[T] = {
    MultiSet(toList ::: other.toList)
  }

  //Intersection by keys
  def &(other: MultiSet[T]): MultiSet[T] = {
    MultiSet(toList.filter(x => other.toList.contains(x)) ::: other.toList.filter(x => toList.contains(x)))
  }

  def apply(key: T): Int = if (myHead != null) {
    val node = myHead.get(key)
    if (node != null) {
      node.total
    } else {
      0
    }
  } else {
    0
  }

  override def toString: String = if (myHead != null) {
    myHead.toList.map(node => s"${node._1} -> ${node._2}").mkString("[", ", ", "]")
  } else {
    "[]"
  }

  private def toList: List[T] = if (myHead != null) {
    var result: List[T] = Nil
    myHead.toList.foreach(value => for (_ <- 1 to value._2) {
      result = result ::: (value._1 :: Nil)
    })
    result
  } else {
    Nil
  }

  def size(): Int = mySize
}

object MultiSet {
  def apply[U]()(implicit ord: U => Ordered[U]) = new MultiSet[U]()

  def apply[U](values: U*)(implicit ord: U => Ordered[U]): MultiSet[U] = {
    var result: List[U] = Nil
    for (value <- values) {
      result = value :: result
    }
    MultiSet(result)
  }

  def apply[U](values: List[U])(implicit ord: U => Ordered[U]): MultiSet[U] = {
    val result = new MultiSet[U]
    values.foreach(result.add)
    result
  }

  def empty[U](implicit ord: U => Ordered[U]): MultiSet[U] = MultiSet[U]()
}
