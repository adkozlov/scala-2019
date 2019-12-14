package ru.spbau.jvm.scala.lecture06

sealed trait NonNegative {
  val value: Int
}

case object NZero extends NonNegative {
  override val value: Int = 0
}

class Next[Prev <: NonNegative](prev: Prev) extends NonNegative {
  override val value: Int = prev.value + 1
}

object Next {
  def apply[Prev <: NonNegative](prev: Prev): Next[Prev] = new Next(prev)
}