package ru.spbau.jvm.scala.db.tables.rows

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

trait RowFabric {
  val DATE_TIME_PATTERN: DateTimeFormatter =
    DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")

  def create(cols: Array[String]): Row
}
