package ru.spbau.jvm.scala.hlist

sealed trait Nonnegative

final case class Succ[
  MinusOne <: Nonnegative
](minusOne: MinusOne) extends Nonnegative

case object Zero extends Nonnegative
