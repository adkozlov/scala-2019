package ru.spbau.jvm.scala.lecture06

trait Nat

case class Succ[P <: Nat](nat: P) extends Nat

case object Zero extends Nat
