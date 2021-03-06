package ru.spbau.jvm.scala
package lecture06

sealed trait HList

final case class HCons[
  +Head,
  +Tail <: HList
](head: Head, tail: Tail) extends HList

case object HNil extends HList
