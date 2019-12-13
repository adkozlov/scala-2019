package ru.spbau.jvm.scala

sealed trait Nat

final case class Suc[Prev <: Nat](n : Prev) extends Nat

case object Zero extends Nat
