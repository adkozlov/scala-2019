package ru.spbau.jvm.scala

sealed trait Peano
object Peano {

  final case class S[+N <: Peano](n: N) extends Peano

  case object Z extends Peano

}
