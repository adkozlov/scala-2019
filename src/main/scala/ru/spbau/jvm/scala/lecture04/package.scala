package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.lecture03.{::, List, Nil}

package object lecture04 {

  // extension methods
  implicit class ListExt[A](private val list: List[A]) extends AnyVal {

    def length: Int = list match {
      case Nil => 0
      case ::(_, tail) => 1 + tail.length
    }

    def withFilter(predicate: A => Boolean): List[A] = list match {
      case Nil => Nil
      case ::(head, tail) =>
        val filteredTail = tail.withFilter(predicate)
        if (predicate(head))
          head :: filteredTail
        else
          filteredTail
    }
  }

  // implicit conversion
  implicit def token2token(token: libA.Token): libB.Token = libB.Token(token.text)
}
