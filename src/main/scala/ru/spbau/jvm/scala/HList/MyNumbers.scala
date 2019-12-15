package ru.spbau.jvm.scala.HList

sealed trait MyNumber

final case class Inc[P <: MyNumber](dec: P) extends MyNumber

case object Zero extends MyNumber



