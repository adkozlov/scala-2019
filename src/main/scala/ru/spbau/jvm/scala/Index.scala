package ru.spbau.jvm.scala

sealed trait Index

case object Zero extends Index

case class Nat[N <: Index](n: N) extends Index
