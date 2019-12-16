package ru.spbau.jvm.scala

sealed trait Nat

case object Zero extends Nat

final case class Suc[Prev <: Nat](n : Prev) extends Nat

