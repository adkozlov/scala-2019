package ru.spbau.jvm.scala.multiset

import scala.util.Random

class Treap[K, V >: Null](implicit ord: K => Ordered[K]) extends Tree[K, V] {

  class Node(key: K, var value: V,
             val balance: Int = Random.nextInt(),
             var right: Node = null,
             var left: Node = null)
            (implicit ord: Ordering[K]) {

    def add(key: K, value: V): Node = {
      if (this.key == key) {
        this.value = value
        return this
      }

      if (ord.lt(key, this.key)) {
        left = if (left == null) {
          new Node(key, value)
        } else {
          left.add(key, value)
        }

        if (left.balance > balance) {
          return rotateLeft()
        }
      } else {
        right = if (right == null) {
          new Node(key, value)
        } else {
          right.add(key, value)
        }

        if (right.balance > balance) {
          return rotateRight()
        }
      }
      this
    }

    def get(key: K): Node = {
      if (this.key == key) {
        return this
      }
      if (ord.lt(this.key, key)) {
        if (right != null) {
          return right.get(key)
        }
        return null
      }

      if (left != null) {
        left.get(key)
      } else null
    }

    def remove(key: K): Node = {
      if (key == this.key) {
        if (left == null) {
          return right
        }
        if (right == null) {
          return left
        }

        if (left.balance < right.balance) {
          val removeFrom = rotateRight()
          removeFrom.left = removeFrom.left.remove(key)
          return removeFrom
        } else {
          val removeFrom = rotateLeft()
          removeFrom.right = removeFrom.right.remove(key)
          return removeFrom
        }
      }

      if (left != null && ord.lt(key, this.key)) {
        left = left.remove(key)
      }
      if (right != null && ord.gt(key, this.key)) {
        right = right.remove(key)
      }

      this
    }

    private def rotateLeft(): Node = {
      val prevLeft = this.left
      val prevLeftRight = prevLeft.right
      this.left = prevLeftRight
      prevLeft.right = this

      prevLeft
    }

    private def rotateRight(): Node = {
      val prevRight = this.right
      val prevRightLeft = prevRight.left
      this.right = prevRightLeft
      prevRight.left = this

      prevRight
    }

    def toList: List[(K, V)] = {
      val leftList = if(this.left != null) this.left.toList else List.empty
      val rightList = if(this.right != null) this.right.toList else List.empty
      leftList ++ List((this.key, this.value)) ++ rightList
    }

    def iterator(): Iterator[(K, V)] = toList.iterator
  }

  private var head: Node = null
  private var treapSize: Int = 0

  override def add(key: K, elem: V): Unit = {
    if (head == null) {
      treapSize += 1
      head = new Node(key, elem)
    } else {
      treapSize += 1
      head = head.add(key, elem)
    }
  }

  override def get(key: K): V = {
    if (head == null) {
      null
    } else {
      val node = head.get(key)
      if (node == null) {
        return null
      }
      node.value
    }
  }

  override def remove(key: K): Unit = {
    if (head != null) {
      head = head.remove(key)
      treapSize -= 1
    }
  }

  override def changeValue(key: K, newValue: V): Unit = {
    if (head != null) {
      val nodeWithKey = head.get(key)
      if (nodeWithKey != null) {
        nodeWithKey.value = newValue
        return
      }
    }
    throw new IllegalArgumentException(s"Treap doesnt contain key $key")
  }

  override def size(): Int = treapSize

  override def toList: List[(K, V)] = if (head == null) {
    List.empty
  } else {
    head.toList
  }

  override def iterator(): Iterator[(K, V)] = if (head != null) {
    head.iterator()
  } else {
    Iterator.empty
  }
}

