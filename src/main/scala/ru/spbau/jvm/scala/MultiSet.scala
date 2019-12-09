package ru.spbau.jvm.scala

import java.util.StringJoiner

import ru.spbau.jvm.scala.MultiSet.Node.ParentHeight
import ru.spbau.jvm.scala.MultiSet.{Tree, _}

import scala.math.max

/**
 * Functional persistent MultiSet which is implemented as AVL-tree.
 */
final class MultiSet[A] private(private val root: Node[A], val size: Int)(implicit ord: Ordering[A]) {

  outer =>

  import ord._

  /**
   * Returns a new multiset with the specified element added.
   */
  def +(element: A): MultiSet[A] = this + (element, 1)

  /**
   * Returns a new multiset without one occurrence of the specified element.
   * If this multiset does not contain that element then returns the same multiset.
   */
  def -(element: A): MultiSet[A] = delImpl(root, element, 1) match {
    case (r, true) => new MultiSet(r, size - 1)
    case (_, false) => this
  }

  /**
   * Same as [[MultiSet.count]].
   */
  def apply(element: A): Int = count(element)

  /**
   * Returns the number of occurrences of the specified element.
   */
  def count(element: A): Int = countImpl(root, element)

  /**
   * Checks whether this multiset contains the specified element.
   */
  def contains(element: A): Boolean = count(element) > 0

  /**
   * Applies a binary operator to a start entry (element, occurrences) and all entries in ascending order.
   * Returns accumulated result.
   */
  def foldLeftPairs[B](container: B)(op: (B, (A, Int)) => B): B = foldLeftImpl(container, root)(op)

  /**
   * Applies a binary operator to a start element and all elements (same element may occur multiple times)
   * in ascending order. Returns accumulated result.
   */
  def foldLeft[B](container: B)(op: (B, A) => B): B = foldLeftImpl(container, root) { (container, value) =>
    val (element, cnt) = value
    var newContainer = container
    for (_ <- 0 until cnt) {
      newContainer = op(newContainer, element)
    }
    newContainer
  }

  /**
   * Apply f to each element.
   */
  def foreach[U](f: A => U): Unit = foldLeft(()) { (_, element) =>
    f(element)
  }

  /**
   * Builds a new correctly sorted MultiSet by applying a function to all elements of this multiset.
   */
  def map[B](f: A => B)(implicit ordering: Ordering[B]): MultiSet[B] = foldLeft(MultiSet[B]()) { (set, element) =>
    set + f(element)
  }

  /**
   * Builds a new correctly sorted MultiSet by applying a function to all elements of this multiset
   * and using the elements of the resulting multisets.
   */
  def flatMap[B](f: A => MultiSet[B])(implicit ev: Ordering[B]): MultiSet[B] =
    foldLeft(MultiSet[B]()) { (set, element) =>
      f(element).foldLeft(set) { (set, mappedElement) =>
        set + mappedElement
      }
    }

  /**
   * Selects all elements of this multiset which satisfy a predicate and builds a new multiset from them.
   */
  def filter(f: A => Boolean): MultiSet[A] = foldLeft(MultiSet()) { (set, element) =>
    if (f(element)) set + element else set
  }

  /**
   * Creates a non-strict filter of this set.
   * Restricts the domain of subsequent map, flatMap, foreach, and withFilter operations.
   */
  def withFilter(p: A => Boolean): MultiSetWithFilter = new MultiSetWithFilter(p)

  /**
   * Computes the union between this multiset and another multiset.
   * Each element x will occur (this.count(x) + that.count(x)) times.
   */
  def |(that: MultiSet[A]): MultiSet[A] = {
    if (size < that.size) {
      return that | this
    }
    that.foldLeftPairs(this) { (set, value: (A, Int)) =>
      set + value
    }
  }

  /**
   * Computes the intersection between this multiset and another multiset.
   * Each element x will occur min(this.count(x), that.count(x)) times.
   */
  def &(that: MultiSet[A]): MultiSet[A] = {
    if (size < that.size) {
      return that & this
    }
    that.foldLeftPairs(MultiSet()) { (set, value: (A, Int)) =>
      val (element, cnt) = value
      val minCnt = math.min(cnt, count(element))
      set + (element, minCnt)
    }
  }

  private def +(value: (A, Int)): MultiSet[A] = value match {
    case (_, -1) => throw new IllegalArgumentException()
    case (_, 0) => this
    case (element, n) => new MultiSet(addImpl(root, element, n), size + n)
  }

  override def toString: String = foldLeftPairs(new StringJoiner(", ", "[", "]")) {
    (sj, value) =>
      val (element, cnt) = value
      sj.add(s"$element -> $cnt")
  }.toString

  private def addImpl(node: Node[A], element: A, n: Int): Node[A] = node match {
    case Nil => Tree(Nil, element -> n, Nil)
    case Tree(l, e -> cnt, r) if e > element =>
      val left = addImpl(l, element, n)
      balance(Tree(left, e -> cnt, r))
    case Tree(l, e -> cnt, r) if e < element =>
      val right = addImpl(r, element, n)
      balance(Tree(l, e -> cnt, right))
    case Tree(l, e -> cnt, r) => Tree(l, e -> (cnt + n), r)
  }

  private def delImpl(node: Node[A], element: A, n: Int): (Node[A], Boolean) = node match {
    case Nil => (Nil, false)
    case Tree(l, e -> cnt, r) if e > element =>
      val (left, deleted) = delImpl(l, element, n)
      balance(Tree(left, e -> cnt, r)) -> deleted
    case Tree(l, e -> cnt, r) if e < element =>
      val (right, deleted) = delImpl(r, element, n)
      balance(Tree(l, e -> cnt, right)) -> deleted
    // e == element
    case Tree(l, _ -> cnt, Nil) if cnt <= n => l -> true
    case Tree(l, _ -> cnt, r: Tree[A]) if cnt <= n =>
      val (replacement, replacementCnt) = leftmost(r)
      val (right, _) = delImpl(r, replacement, replacementCnt)
      balance(Tree(l, replacement -> replacementCnt, right)) -> true
    // e == element && cnt > n
    case Tree(l, e -> cnt, r) => Tree(l, e -> (cnt - n), r) -> true
  }

  @scala.annotation.tailrec
  private def leftmost(tree: Tree[A]): (A, Int) = tree match {
    case Tree(Nil, value, _) => value
    case Tree(l: Tree[A], _, _) => leftmost(l)
  }

  @scala.annotation.tailrec
  private def countImpl(node: Node[A], element: A): Int = node match {
    case Nil => 0
    case Tree(_, e -> cnt, _) if e == element => cnt
    case Tree(l, e -> _, _) if e > element => countImpl(l, element)
    case Tree(_, e -> _, r) if e < element => countImpl(r, element)
  }

  private def balance(node: Tree[A]): Tree[A] = {
    implicit val parentHeight: ParentHeight = new ParentHeight {
      override val h: Int = node.height
    }

    node match {
      case Tree(Tree(Node(1), _, Node(0)), _, Node(0)) => rotateLeftLeft(node)
      case Tree(Tree(Node(0), _, Node(1)), _, Node(0)) => rotateLeftRight(node)
      case Tree(Node(0), _, Tree(Node(0), _, Node(1))) => rotateRightRight(node)
      case Tree(Node(0), _, Tree(Node(1), _, Node(0))) => rotateRightLeft(node)
      case _ => node
    }
  }

  private def rotateLeftLeft(node: Tree[A]): Tree[A] = {
    val Tree(Tree(Tree(t1, x, t2), y, t3), z, t4) = node
    Tree(Tree(t1, x, t2), y, Tree(t3, z, t4))
  }

  private def rotateLeftRight(node: Tree[A]): Tree[A] = {
    val Tree(Tree(t1, y, Tree(t2, x, t3)), z, t4) = node
    Tree(Tree(t1, y, t2), x, Tree(t3, z, t4))
  }

  private def rotateRightRight(node: Tree[A]): Tree[A] = {
    val Tree(t1, z, Tree(t2, y, Tree(t3, x, t4))) = node
    Tree(Tree(t1, z, t2), y, Tree(t3, x, t4))
  }

  private def rotateRightLeft(node: Tree[A]): Tree[A] = {
    val Tree(t1, z, Tree(Tree(t2, x, t3), y, t4)) = node
    Tree(Tree(t1, z, t2), x, Tree(t3, y, t4))
  }

  private def foldLeftImpl[B](container: B, root: Node[A])(op: (B, (A, Int)) => B): B = root match {
    case Nil => container
    case Tree(l, value, r) =>
      val leftContainer = foldLeftImpl(container, l)(op)
      foldLeftImpl(op(leftContainer, value), r)(op)
  }

  class MultiSetWithFilter(p: A => Boolean) {
    def map[B](f: A => B)(implicit ordering: Ordering[B]): MultiSet[B] = outer filter p map f

    def flatMap[B](f: A => MultiSet[B])(implicit ordering: Ordering[B]): MultiSet[B] = outer filter p flatMap f

    def foreach[U](f: A => U): Unit = outer filter p foreach f

    def withFilter(q: A => Boolean): MultiSetWithFilter = new MultiSetWithFilter(x => p(x) && q(x))
  }

}

private object MultiSet {

  sealed trait Node[+A] {
    def height: Int
  }

  object Node {

    trait ParentHeight {
      val h: Int
    }

    def unapply(arg: Node[_])(implicit parentHeight: ParentHeight): Option[Int] =
      Some(arg.height - (parentHeight.h - 3))
  }

  case class Tree[A](l: Node[A], value: (A, Int), r: Node[A]) extends Node[A] {
    override val height: Int = max(l.height, r.height) + 1
  }

  case object Nil extends Node[Nothing] {
    override def height: Int = 0
  }

  def apply[A](elements: A*)(implicit ord: Ordering[A]): MultiSet[A] = {
    elements.foldLeft(new MultiSet[A](Nil, 0)) { (set, elem) =>
      set + elem
    }
  }
}


