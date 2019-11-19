package ru.spbau.jvm.scala.cli.commands

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import ru.spbau.jvm.scala.db.DB

trait Command {
  val DATE_TIME_PATTERN: DateTimeFormatter =
    DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")

  def exec(db: DB, args: Array[String]): String
}
