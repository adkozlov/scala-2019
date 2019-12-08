package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.MultiSet.N.ParentHeight
import ru.spbau.jvm.scala.MultiSet.{T, _}

import scala.math.max

final class MultiSet[A](private val root: N[A], private val size: Int)(implicit ord: Ordering[A]) {

  import ord._

  def +(element: A): MultiSet[A] = new MultiSet(addImpl(root, element), size + 1)

  def -(element: A): MultiSet[A] = delImpl(root, element, 1) match {
    case (r, true) => new MultiSet(r, size - 1)
    case (r, false) => new MultiSet(r, size)
  }

  def count(element: A): Int = countImpl(root, element)

  def contains(element: A): Boolean = count(element) > 0

  private def addImpl(node: N[A], element: A): N[A] = node match {
    case Nil => T(Nil, element -> 1, Nil)
    case T(l, e -> cnt, r) if e > element =>
      val left = addImpl(l, element)
      balance(T(left, e -> cnt, r))
    case T(l, e -> cnt, r) if e < element =>
      val right = addImpl(r, element)
      balance(T(l, e -> cnt, right))
    case T(l, e -> cnt, r) => T(l, e -> (cnt + 1), r)
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

}


