package ru.spbau.jvm.scala.heterogeneous

sealed trait Number

final case class Succ[Pred <: Number](pred: Pred) extends Number

case object Zero extends Number
