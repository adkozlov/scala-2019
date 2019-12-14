package ru.spbau.jvm.scala.lecture06

sealed trait Natural

case class Succ[Prev <: Natural](prev: Prev) extends Natural

case object Zero extends Natural