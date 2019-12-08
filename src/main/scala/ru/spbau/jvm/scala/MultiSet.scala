package ru.spbau.jvm.scala

import java.util.StringJoiner

import ru.spbau.jvm.scala.MultiSet.N.ParentHeight
import ru.spbau.jvm.scala.MultiSet.{T, _}

import scala.math.max

/**
 * Functional persistent MultiSet which is implemented as AVL-tree.
 */
final class MultiSet[A] private(private val root: N[A], val size: Int)(implicit ord: Ordering[A]) {

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

  private def addImpl(node: N[A], element: A, n: Int): N[A] = node match {
    case Nil => T(Nil, element -> n, Nil)
    case T(l, e -> cnt, r) if e > element =>
      val left = addImpl(l, element, n)
      balance(T(left, e -> cnt, r))
    case T(l, e -> cnt, r) if e < element =>
      val right = addImpl(r, element, n)
      balance(T(l, e -> cnt, right))
    case T(l, e -> cnt, r) => T(l, e -> (cnt + n), r)
  }

  private def delImpl(node: N[A], element: A, n: Int): (N[A], Boolean) = node match {
    case Nil => (Nil, false)
    case T(l, e -> cnt, r) if e > element =>
      val (left, deleted) = delImpl(l, element, n)
      balance(T(left, e -> cnt, r)) -> deleted
    case T(l, e -> cnt, r) if e < element =>
      val (right, deleted) = delImpl(r, element, n)
      balance(T(l, e -> cnt, right)) -> deleted
    // e == element
    case T(l, _ -> cnt, Nil) if cnt <= n => l -> true
    case T(l, _ -> cnt, r: T[A]) if cnt <= n =>
      val (replacement, replacementCnt) = leftmost(r)
      val (right, _) = delImpl(r, replacement, replacementCnt)
      balance(T(l, replacement -> replacementCnt, right)) -> true
    // e == element && cnt > n
    case T(l, e -> cnt, r) => T(l, e -> (cnt - n), r) -> true
  }

  @scala.annotation.tailrec
  private def leftmost(tree: T[A]): (A, Int) = tree match {
    case T(Nil, value, _) => value
    case T(l: T[A], _, _) => leftmost(l)
  }

  @scala.annotation.tailrec
  private def countImpl(node: N[A], element: A): Int = node match {
    case Nil => 0
    case T(_, e -> cnt, _) if e == element => cnt
    case T(l, e -> _, _) if e > element => countImpl(l, element)
    case T(_, e -> _, r) if e < element => countImpl(r, element)
  }

  private def balance(node: T[A]): T[A] = {
    implicit val parentHeight: ParentHeight = new ParentHeight {
      override val h: Int = node.height
    }

    node match {
      case T(T(N(1), _, N(0)), _, N(0)) => rotateLeftLeft(node)
      case T(T(N(0), _, N(1)), _, N(0)) => rotateLeftRight(node)
      case T(N(0), _, T(N(0), _, N(1))) => rotateRightRight(node)
      case T(N(0), _, T(N(1), _, N(0))) => rotateRightLeft(node)
      case _ => node
    }
  }

  private def rotateLeftLeft(node: T[A]): T[A] = {
    val T(T(T(t1, x, t2), y, t3), z, t4) = node
    T(T(t1, x, t2), y, T(t3, z, t4))
  }

  private def rotateLeftRight(node: T[A]): T[A] = {
    val T(T(t1, y, T(t2, x, t3)), z, t4) = node
    T(T(t1, y, t2), x, T(t3, z, t4))
  }

  private def rotateRightRight(node: T[A]): T[A] = {
    val T(t1, z, T(t2, y, T(t3, x, t4))) = node
    T(T(t1, z, t2), y, T(t3, x, t4))
  }

  private def rotateRightLeft(node: T[A]): T[A] = {
    val T(t1, z, T(T(t2, x, t3), y, t4)) = node
    T(T(t1, z, t2), x, T(t3, y, t4))
  }

  private def foldLeftImpl[B](container: B, root: N[A])(op: (B, (A, Int)) => B): B = root match {
    case Nil => container
    case T(l, value, r) =>
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

  sealed trait N[+A] {
    def height: Int
  }

  object N {

    trait ParentHeight {
      val h: Int
    }

    def unapply(arg: N[_])(implicit parentHeight: ParentHeight): Option[Int] = Some(arg.height - (parentHeight.h - 3))
  }

  case class T[A](l: N[A], value: (A, Int), r: N[A]) extends N[A] {
    override val height: Int = max(l.height, r.height) + 1
  }

  case object Nil extends N[Nothing] {
    override def height: Int = 0
  }

  def apply[A](elements: A*)(implicit ord: Ordering[A]): MultiSet[A] = {
    elements.foldLeft(new MultiSet[A](Nil, 0)) { (set, elem) =>
      set + elem
    }
  }
}


