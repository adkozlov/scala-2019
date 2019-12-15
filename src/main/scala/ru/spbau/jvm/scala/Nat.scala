package ru.spbau.jvm.scala

sealed trait Nat

final case class Succ[T <: Nat](prev: T) extends Nat

case object Zero extends Nat
