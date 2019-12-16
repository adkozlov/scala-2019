package ru.spbau.jvm.scala.hlist

sealed trait Number

final case class Succ[
+Prev <: Number
](prev: Prev) extends Number

case object Zero extends Number
