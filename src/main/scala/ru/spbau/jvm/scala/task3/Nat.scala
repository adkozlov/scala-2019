package ru.spbau.jvm.scala.task3

sealed trait Nat

final case class Suc[T <: Nat](prev: T) extends Nat

case object Zero extends Nat