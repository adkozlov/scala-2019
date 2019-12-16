package ru.spbau.jvm.scala.lecture06

sealed trait Nonnegative

case class Next[
  Prev <: Nonnegative
](prev: Prev) extends Nonnegative

case object Zero extends Nonnegative