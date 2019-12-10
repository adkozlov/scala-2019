package ru.spbau.jvm.scala

/**
 * Bar <: Foo => List[Bar] <: List[Foo]
 */
sealed trait List[+A] {
  def head: A

  def tail: List[A]

  def isEmpty: Boolean

  def :+[B >: A](last: B): List[B]

  // polymorphic implementation
  def foreach(function: A => Unit): Unit

  def withFilter(f: A => Boolean): List[A]

  final def +:[B >: A](head: B): List[B] =
    ::(head)

  final def ::[B >: A](head: B): List[B] =
    new ::(head, this)

  final def :::[B >: A](prefix: List[B]): List[B] = prefix match {
    case Nil => this
    case ::(head, tail) => head :: tail ::: this
  }

  // pattern-matching implementation
  final def map[B](function: A => B): List[B] = this match {
    case Nil => Nil
    case ::(head, tail) => function(head) :: tail.map(function)
  }
}

final case class ::[+A](head: A, tail: List[A]) extends List[A] {

  override def isEmpty = false

  override def :+[B >: A](last: B): List[B] =
    head :: (tail :+ last)

  override def foreach(function: A => Unit): Unit = {
    function(head) // function.apply(head)
    tail.foreach(function)
  }

  override def withFilter(f: A => Boolean): List[A] = {
    var result: List[A] = Nil
    this.foreach { element => if (f(element)) result = element :: result }
    result
  }
}

/**
 * [[Nothing]] <: \forall type T <: [[Any]]
 */
case object Nil extends List[Nothing] {

  override def head = throw new NoSuchElementException

  override def tail = throw new UnsupportedOperationException

  override def isEmpty = true

  override def :+[B >: Nothing](last: B): List[B] =
    last :: Nil

  override def foreach(function: Nothing => Unit): Unit = {}

  override def withFilter(f: Nothing => Boolean): List[Nothing] = Nil

  override def toString(): String = {
    "NIL"
  }
}
