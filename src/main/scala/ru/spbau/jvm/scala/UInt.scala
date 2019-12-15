package ru.spbau.jvm.scala

sealed trait UInt

case object Nil extends UInt

case class Number[+Tail <: UInt](tail: Tail) extends UInt