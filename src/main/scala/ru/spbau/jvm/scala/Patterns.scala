package ru.spbau.jvm.scala

import scala.util.matching.Regex

object Patterns {

  val calls: Regex = "^calls from ([0-9.]+) to ([0-9.]+)$".r
  val avg: Regex = "^avg$".r
  val totalFromTo: Regex = "^total from ([0-9.]+) to ([0-9.]+)$".r
  val totalFrom: Regex = "^total from ([0-9.]+)$".r
  val total: Regex = "^total$".r
  val totalBy: Regex = "^total by ([A-Za-z]+) ([A-Za-z]+)$".r
  val maxTotal: Regex = "^max total$".r
  val number: Regex = "^number ([A-Za-z]+) ([A-Za-z]+)$".r
  val name: Regex = "^name ([0-9+]+)$".r
  val help: Regex = "^help$".r
  val exit: Regex = "^exit$".r

}
