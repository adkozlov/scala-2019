package ru.spbau.jvm.scala.commands

import java.time.LocalDateTime
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

import ru.spbau.jvm.scala.User

object ParserUtil {
  def parseInterval(array: Array[String]): (LocalDateTime, LocalDateTime) = {
    val fmt = new DateTimeFormatterBuilder()
      .appendPattern("dd.MM.yyyy")
      .optionalStart()
      .appendPattern("-HH:mm")
      .optionalEnd()
      .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
      .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
      .toFormatter()
    array match {
      case Array() => (LocalDateTime.MIN, LocalDateTime.MAX)
      case Array("from", from) => (LocalDateTime.parse(from, fmt), LocalDateTime.MAX)
      case Array("from", from, "to", to) => (LocalDateTime.parse(from, fmt), LocalDateTime.parse(to, fmt))
      case Array("to", to) => (LocalDateTime.MIN, LocalDateTime.parse(to, fmt))
      case _ => throw new IllegalArgumentException("Parse error")
    }
  }

  def parseUser(array: Array[String]): User = {
    array match {
      case Array(firstName, lastName) => User(firstName, lastName)
      case _ => throw new IllegalArgumentException("Parse error")
    }
  }
}
