package ru.spbau.jvm.scala
package lecture06

sealed trait Index

final case class AtLeastOneIndex[+Tail <: Index](remains: Tail) extends Index

case object NilIndex extends Index