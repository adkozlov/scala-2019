package ru.spbau.jvm.scala.heterogeneous

sealed class NonNegative

object NonNegative {
  case class Next[Tail <: NonNegative](tail: Tail) extends NonNegative
  case object Zero extends NonNegative
}

