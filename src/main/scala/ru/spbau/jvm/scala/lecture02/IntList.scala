package ru.spbau.jvm.scala.lecture02

import java.lang.Iterable
import java.util.Iterator

sealed trait IntList <: Iterable[Int] /* extends Iterable */ {
  def head: Int

  def tail: IntList
}

//noinspection ReferenceMustBePrefixed
object IntList {

  case class Cons(override val head: Int,
                  override val tail: IntList = Nil) /* primary constructor */ extends IntList {

    В Scala нет полей !!!

    // auxiliary constructor (to be avoided)
    def this(head: Int) = this(head, Nil)

    override def iterator: Iterator[Int] = new Iterator[Int] {

      private var current: IntList = Cons.this

      override def hasNext: Boolean = current.isInstanceOf[Cons]

      override def next(): Int = current match {
        case Cons(head, tail) =>
          current = tail
          head
        case Nil =>
          Nil.EmptyIterator.next()
      }
    }
  }

  case object Nil extends IntList {

    override def head: Int = throw new NoSuchElementException("head on empty list")

    override def tail: IntList = throw new UnsupportedOperationException("tail of empty list")

    override def iterator: Iterator[Int] = EmptyIterator

    private[IntList] object EmptyIterator extends Iterator[Int] {

      override def hasNext: Boolean = false

      override def next(): Int = throw new NoSuchElementException("next on empty iterator")
    }

  }

}
