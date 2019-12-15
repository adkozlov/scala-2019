package ru.spbau.jvm.scala

sealed class Nat

case object Zero extends Nat

case class Suc[N <: Nat](n: N) extends Nat