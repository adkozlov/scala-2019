package ru.spbau.jvm.scala.hlist

sealed trait Natural

case object Zero extends Natural

case class Next[N <: Natural](prev: N) extends Natural
