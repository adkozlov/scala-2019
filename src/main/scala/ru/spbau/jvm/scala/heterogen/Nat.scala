package ru.spbau.jvm.scala.heterogen

sealed trait Nat

case class Suc[T <: Nat](pred: T) extends Nat

case object Zero extends Nat
