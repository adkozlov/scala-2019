package ru.spbau.jvm.scala
package lecture06

sealed trait Nat

case class Succ[Prev <: Nat](prev: Prev) extends Nat

case object Nat0 extends Nat