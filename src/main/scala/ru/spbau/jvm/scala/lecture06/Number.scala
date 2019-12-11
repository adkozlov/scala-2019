package ru.spbau.jvm.scala.lecture06

sealed trait Number

final case class PlusOne[
  +Tail <: Number
](tail: Tail) extends Number

case object Zero extends Number
