package ru.spbau.jvm.scala.multiset

import java.util.StringJoiner

import ru.spbau.jvm.scala.multiset.MultiSet._

final class MultiSet[A](private val root: Tree[A])(implicit ord: Ordering[A]) {
  def count(key: A): Int = count(root, key)

  def apply(key: A): Int = count(key)

  def +(key: A): MultiSet[A] = new MultiSet(insert(root, key))

  def -(key: A): MultiSet[A] = new MultiSet(delete(root, key, all = false, 1))

  def add(key: A, size: Int = 1): MultiSet[A] = new MultiSet(insert(root, key, size))

  def delete(key: A, size: Int = 1): MultiSet[A] = new MultiSet(delete(root, key, all = false, size))

  def deleteAll(key: A): MultiSet[A] = new MultiSet(delete(root, key, all = true, 1))

  def foreach[B](f: A => B): Unit =
    foldLeftElements((_: Unit, key) => f(key) : Unit, ())

  def map[B](f: A => B)(implicit ord: Ordering[B]): MultiSet[B] =
    foldLeft((acc: MultiSet[B], key, size) => acc.add(f(key), size), MultiSet())

  def filter(f: A => Boolean): MultiSet[A] =
    foldLeft((acc: MultiSet[A], key, size) => {
      if (f(key))
        acc.add(key, size)
      else
        acc
    }, MultiSet())

  def toList: List[(A, Int)] =
    foldRight((key: A, size: Int, acc: List[(A, Int)]) => (key, size) :: acc, List())

  override def toString: String =
    foldLeft(
      (acc: StringJoiner, key, size) => acc.add(s"$key -> $size"),
      new StringJoiner(", ", "[", "]")
    ).toString

  def |(other: MultiSet[A]): MultiSet[A] = {
    val (first, second) = if (treeSize(this.root) < treeSize(other.root)) (this, other) else (other, this)
    first.foldLeft((acc: MultiSet[A], key, size) => acc.add(key, size), second)
  }

  def &(other: MultiSet[A]): MultiSet[A] = {
    val (first, second) = if (treeSize(this.root) < treeSize(other.root)) (this, other) else (other, this)
    first.foldLeft((acc: MultiSet[A], key, size) => {
      val minSize = math.min(size, second(key))
      if (minSize > 0)
        acc.add(key, minSize)
      else
        acc
    }, MultiSet())
  }

  private def foldLeft[C](f: (C, A, Int) => C, acc: C): C =
    foldLeft(root, f, acc)

  private def foldLeftElements[C](f: (C, A) => C, acc: C): C =
    foldLeft((acc, key, _) => f(acc, key), acc)

  private def foldRight[C](f: (A, Int, C) => C, acc: C): C =
    foldRight(root, f, acc)

  private def insert(tree: Tree[A], key: A, count: Int = 1): Tree[A] = tree match {
    case Nil =>
      Node(Element(key, count))
    case Node(element, left, right) =>
      if (ord.lt(key, element.key))
        balance(Node(element, insert(left, key, count), right))
      else if (ord.gt(key, element.key))
        balance(Node(element, left, insert(right, key, count)))
      else
        Node(Element(key, element.size + count), left, right)
  }

  private def delete(tree: Tree[A], key: A, all: Boolean, count: Int): Tree[A] = tree match {
    case Nil =>
      Nil
    case Node(element, left, right) =>
      if (ord.lt(key, element.key))
        balance(Node(element, delete(left, key, all, count), right))
      else if (ord.gt(key, element.key))
        balance(Node(element, left, delete(right, key, all, count)))
      else if (all || element.size <= count) {
        right match {
          case Nil =>
            left
          case node @ Node(_, _, _) =>
            val (min, tree) = deleteMin(node)
            balance(Node(min, left, tree))
        }
      } else
        Node(Element(key, element.size - count), left, right)
  }

  @scala.annotation.tailrec
  private def count(tree: Tree[A], key: A): Int = tree match {
    case Nil =>
      0
    case Node(element, left, right) =>
      if (ord.lt(key, element.key))
        count(left, key)
      else if (ord.gt(key, element.key))
        count(right, key)
      else
        element.size
  }

  private def foldLeft[C](tree: Tree[A], f: (C, A, Int) => C, acc: C): C = tree match {
    case Nil =>
      acc
    case Node(Element(key, size), left, right) =>
      foldLeft(right, f, f(foldLeft(left, f, acc), key, size))
  }

  private def foldRight[C](tree: Tree[A], f: (A, Int, C) => C, acc: C): C = tree match {
    case Nil =>
      acc
    case Node(Element(key, size), left, right) =>
      foldRight(left, f, f(key, size, foldRight(right, f, acc)))
  }

  private def treeSize(tree: Tree[A]): Int =
    foldLeftElements((acc: Int, _: A) => acc + 1, 0)

  private def balance(tree: Tree[A]): Tree[A] = tree match {
    case Nil =>
      Nil
    case node @ Node(_, left, right) => node.balanceFactor match {
      case 2 if right.balanceFactor >= 0 =>
        leftRotate(node)
      case 2 =>
        leftRotateBig(node)
      case -2 if left.balanceFactor <= 0 =>
        rightRotate(node)
      case -2 =>
        rightRotateBig(node)
      case _ =>
        node
    }
  }

  private def leftRotate(node: Node[A]): Tree[A] = node match {
    case Node(p, a, Node(q, b, c)) => Node(q, Node(p, a, b), c)
    case _ => throw new RuntimeException // never happens
  }

  private def rightRotate(node: Node[A]): Tree[A] = node match {
    case Node(q, Node(p, a, b), c) => Node(p, a, Node(q, b, c))
    case _ => throw new RuntimeException // never happens
  }

  private def leftRotateBig(node: Node[A]): Tree[A] = node match {
    case Node(p, a, Node(q, Node(s, b, c), d)) => Node(s, Node(p, a, b), Node(q, c, d))
    case _ => throw new RuntimeException // never happens
  }

  private def rightRotateBig(node: Node[A]): Tree[A] = node match {
    case Node(q, Node(p, a, Node(s, b, c)), d) => Node(s, Node(p, a, b), Node(q, c, d))
    case _ => throw new RuntimeException // never happens
  }

  private def deleteMin(node: Node[A]): (Element[A], Tree[A]) = node match {
    case Node(element, left, right) =>
      left match {
        case Nil =>
          (element, right)
        case node @ Node(_, _, _) =>
          val (min, tree) = deleteMin(node)
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
