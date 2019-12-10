package ru.spbau.jvm.scala
package lecture06

sealed trait Nat

case object Zero extends Nat

final case class Succ[N <: Nat](prev: N) extends Nat