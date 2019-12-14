package ru.spbau.jvm.scala.multiset

import java.util.NoSuchElementException

case class DataKey[A](key: A, var count: Int = 1)

class MultiSet[A](implicit val ordering: Ordering[A]) extends Iterable[A] {
  type Data = DataKey[A]

  private var root: Option[Node[Data]] = None

  def findData(key: A): Option[Data] = findNode(key) match {
    case Some(node) => Some(node.value)
    case None => None
  }

  override def isEmpty(): Boolean = root.isEmpty

  def contains(key: A): Boolean = findData(key).isDefined

  def remove(key: A): Unit = findNode(key) match {
    case Some(node) =>
      node.value.count -= 1
      if (node.value.count == 0)
        root = Node.deleteNode(node)
    case None =>
  }

  def add(key: A, count: Int = 1): Unit = {
    if (count < 1) {
      throw new IllegalArgumentException
    }
    root = Some(addNode(DataKey(key, count)))
  }

  def getCount(key: A): Int = {
    findData(key) match {
      case Some(data) => data.count
      case None => 0
    }
  }

  def apply(key: A): Int = getCount(key)

  def &(other: MultiSet[A]): MultiSet[A] = {
    val newMS = new MultiSet[A]()
    val it1 = dataIterator()
    val it2 = other.dataIterator()
    while (it1.hasNext && it2.hasNext) {
      val data1 = it1.current
      val data2 = it2.current
      ordering.compare(data1.key, data2.key) match {
        case 0 =>
          val minCount = scala.math.min(data1.count, data2.count)
          newMS.add(data1.key, minCount)
          it1.next
          it2.next
        case x if x < 0 => it1.next
        case _ => it2.next
      }
    }
    newMS
  }

  def |(other: MultiSet[A]): MultiSet[A] = {
    val newMS = new MultiSet[A]()
    val it1 = dataIterator()
    val it2 = other.dataIterator()
    while (it1.hasNext && it2.hasNext) {
      val data1 = it1.current
      val data2 = it2.current
      ordering.compare(data1.key, data2.key) match {
        case 0 =>
          val sumCount = data1.count + data2.count
          newMS.add(data1.key, sumCount)
          it1.next
          it2.next
        case x if x < 0 =>
          newMS.add(data1.key, data1.count)
          it1.next
        case _ =>
          newMS.add(data2.key, data2.count)
          it2.next
      }
    }

    while (it1.hasNext) {
      val data = it1.next
      newMS.add(data.key, data.count)
    }
    while (it2.hasNext) {
      val data = it2.next
      newMS.add(data.key, data.count)
    }
    newMS
  }

  override def toString: _root_.java.lang.String = {
    val builder = new StringBuilder
    builder.append('[')
    val it = dataIterator()
    while (it.hasNext) {
      val data = it.next()
      builder.append(s"${data.key}->${data.count}")
      if (it.hasNext) builder.append(',')
    }
    builder.append(']')
    builder.toString()
  }

  override def foreach[U](f: A => U): Unit = {
    val it = iterator()
    while (it.hasNext) {
      f(it.next())
    }
  }

  def map[B](f: A => B)(implicit ordering: Ordering[B]): Iterable[B] = {
    val newMS = MultiSet[B]()
    val it = iterator()
    while (it.hasNext) {
      newMS.add(f(it.next()))
    }
    newMS
  }

  override def filter(pred: A => Boolean): MultiSet[A] = {
    val newMS = MultiSet[A]()
    val it = iterator()
    while (it.hasNext) {
      val data = it.next()
      if (pred(data)) {
        newMS.add(data)
      }
    }
    newMS
  }

  private def dataIterator(): MultiSetDataIterator = new MultiSetDataIterator

  class MultiSetDataIterator extends Iterator[DataKey[A]] {
    private var node = findMinNode()

    override def hasNext: Boolean = node.isDefined

    override def next: DataKey[A] = {
      val ret = current
      node = Node.nextNode(node.get)
      ret
    }

    def current: DataKey[A] = node match {
      case Some(node) => node.value
      case None => throw new NoSuchElementException
    }
  }

  override def iterator(): MultiSetIterator = new MultiSetIterator

  class MultiSetIterator extends Iterator[A] {
    private val it = new MultiSetDataIterator
    private var count: Int = if (it.hasNext) it.current.count else 0

    override def hasNext: Boolean = count > 0

    override def next(): A = {
      if (count <= 0) {
        throw new NoSuchElementException
      }
      count -= 1
      if (count > 0) {
        it.current.key
      } else {
        val ret = it.next.key
        count = if (it.hasNext) it.current.count else 0
        ret
      }
    }
  }

  private def addNode(data: Data): Node[Data] = {
    findNodeOrParent(data.key) match {
      case Some(node) =>
        val compareResult = ordering.compare(data.key, node.value.key)
        if (compareResult == 0) {
          node.value.count += data.count
          root.get
        } else {
          val newNode = new Node(data)
          Node.setSon(Some(node), Some(newNode), compareResult > 0)
          node.cascadingBalance()
        }
      case None =>
        new Node(data)
    }
  }

  private def findNodeOrParent(key: A): Option[Node[Data]] = {
    var node = root
    var parent: Option[Node[Data]] = None
    while (node.isDefined) {
      parent = node
      val compareResult = ordering.compare(key, node.get.value.key)
      compareResult match {
        case 0 => return node
        case x if x < 0 => node = node.get.left
        case _ => node = node.get.right
      }
    }
    parent
  }

  private def findNode(key: A): Option[Node[Data]] = findNodeOrParent(key) match {
    case Some(node) => if (ordering.compare(node.value.key, key) == 0) Some(node) else None
    case None => None
  }

  private def findMinNode(): Option[Node[Data]] = {
    var node = root
    while (node.isDefined && node.get.left.isDefined) {
      node = node.get.left
    }
    node
  }
}

object MultiSet {
  def apply[A](values: A*)(implicit ordering: Ordering[A]): MultiSet[A] = {
    val m = new MultiSet[A]
    values.foreach(x => m.add(x))
    m
  }
}
