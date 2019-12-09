package ru.spbau.jvm.scala

import java.util.StringJoiner

import ru.spbau.jvm.scala.MultiSet._

import scala.collection.mutable

final class MultiSet[A](private val root: Tree[A])(implicit ord: Ordering[A]) {
  def count(key: A): Int = count(root, key)

  def apply(key: A): Int = count(key)

  def +(key: A): MultiSet[A] = new MultiSet(insert(root, key))

  def -(key: A): MultiSet[A] = new MultiSet(delete(root, key))

  override def toString: String = {
    val joiner = new StringJoiner(", ", "[", "]")
    toString(root, joiner)
    joiner.toString
  }

  def foreach[B](f: A => B): Unit =
    foreach(root, f)

  def map[B](f: A => B)(implicit ord: Ordering[B]): MultiSet[B] =
    new MultiSet(map(root, f))

  def toList: List[(A, Int)] = {
    val listBuilder = List.newBuilder[(A, Int)]
    toList(root, listBuilder)
    listBuilder.result()
  }

  def filter(f: A => Boolean): MultiSet[A] = {
    var result: Tree[A] = Nil
    for ((element, size) <- toList) {
      result = insert(result, element, size)
    }
    new MultiSet(result)
  }

  private def insert(tree: Tree[A], key: A, count: Int = 1): Tree[A] = tree match {
    case Nil =>
      Node(Element(key, count))
    case Node(element, left, right) =>
      if (ord.lt(key, element.key))
        balance(Node(element, insert(left, key), right))
      else if (ord.gt(key, element.key))
        balance(Node(element, left, insert(right, key)))
      else
        Node(Element(key, element.size + count), left, right)
  }

  private def delete(tree: Tree[A], key: A): Tree[A] = tree match {
    case Nil =>
      Nil
    case Node(element, left, right) => {
      if (ord.lt(key, element.key))
        balance(Node(element, delete(left, key), right))
      else if (ord.gt(key, element.key))
        balance(Node(element, left, delete(right, key)))
      else if (element.size == 1) {
        right match {
          case Nil =>
            left
          case _ =>
            val (min, tree) = deleteMin(right.asInstanceOf[Node[A]])
            balance(Node(min, left, tree))
        }
      } else
        Node(Element(key, element.size - 1), left, right)
    }
  }

  private def count(tree: Tree[A], key: A): Int = tree match {
    case Nil =>
      0
    case Node(element, left, right) => {
      if (ord.lt(key, element.key))
        count(left, key)
      else if (ord.gt(key, element.key))
        count(right, key)
      else
        element.size
    }
  }

  private def foreach[B](tree: Tree[A], f: A => B): Unit = tree match {
    case Node(element, left, right) =>
      foreach(left, f)
      f(element.key)
      foreach(right, f)
  }

  private def map[B](tree: Tree[A], f: A => B): Tree[B] = tree match {
    case Nil =>
      Nil
    case Node(element, left, right) =>
      Node(Element(f(element.key), element.size), map(left, f), map(right, f))
  }

  private def toList(tree: Tree[A], listBuilder: mutable.Builder[(A, Int), List[(A, Int)]]): Unit = tree match {
    case Node(element, left, right) =>
      toList(left, listBuilder)
      listBuilder.addOne((element.key, element.size))
      toList(right, listBuilder)
  }

  private def toString(tree: Tree[A], joiner: StringJoiner): Unit = tree match {
    case Node(element, left, right) =>
      toString(left, joiner)
      joiner.add(s"${element.key} -> ${element.size}")
      toString(right, joiner)
  }

  private def balance(tree: Tree[A]): Tree[A] = tree match {
    case Nil =>
      Nil
    case Node(_, left, right) => {
      case 2 if right.balanceFactor >= 0 =>
        leftRotate(tree.asInstanceOf[Node[A]])
      case 2 =>
        leftRotateBig(tree.asInstanceOf[Node[A]])
      case -2 if left.balanceFactor <= 0 =>
        rightRotate(tree.asInstanceOf[Node[A]])
      case -2 =>
        rightRotateBig(tree.asInstanceOf[Node[A]])
      case _ =>
        tree
    }
  }

  private def leftRotate(node: Node[A]): Tree[A] = node match {
    case Node(p, a, Node(q, b, c)) => Node(q, Node(p, a, b), c)
  }

  private def rightRotate(node: Node[A]): Tree[A] = node match {
    case Node(q, Node(p, a, b), c) => Node(p, a, Node(q, b, c))
  }

  private def leftRotateBig(node: Node[A]): Tree[A] = node match {
    case Node(p, a, Node(q, Node(s, b, c), d)) => Node(s, Node(p, a, b), Node(q, c, d))
  }

  private def rightRotateBig(node: Node[A]): Tree[A] = node match {
    case Node(q, Node(p, a, Node(s, b, c)), d) => Node(s, Node(p, a, b), Node(q, c, d))
  }

  private def deleteMin(node: Node[A]): (Element[A], Tree[A]) = node match {
    case Node(element, left, right) =>
      left match {
        case Nil =>
          (element, right)
        case Node(e, l, r) =>
          val (min, tree) = deleteMin(left.asInstanceOf[Node])
          (min, balance(Node(element, tree, right)))
      }
  }

}

private object MultiSet {
  def apply[A](elements: A*)(implicit ord: Ordering[A]): MultiSet[A] = {
    var root : MultiSet[A] = new MultiSet(Nil)
    for (e <- elements) {
      root = root + e
    }
    root
  }

  sealed trait Tree[+A] {
    def height: Int

    def balanceFactor: Int
  }

  case object Nil extends Tree[Nothing] {
    override def height: Int = 0

    override def balanceFactor: Int = 0
  }

  case class Node[A](element: Element[A], left: Tree[A] = Nil, right: Tree[A] = Nil) extends Tree[A] {
    val height: Int = 1 + math.max(left.height, right.height)

    override def balanceFactor: Int = right.height - left.height
  }

  case class Element[A](key: A, size: Int)
}
