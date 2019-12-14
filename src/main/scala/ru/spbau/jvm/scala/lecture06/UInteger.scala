package ru.spbau.jvm.scala
package lecture06

sealed trait UInteger

final case class Next[+Tail <: UInteger](tail: Tail) extends UInteger

case object Zero extends UInteger