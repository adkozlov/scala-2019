package ru.spbau.jvm.scala.heterogen

sealed trait Nat

case class Suc[T <: Nat](nat: T) extends Nat

case object Zero extends Nat
