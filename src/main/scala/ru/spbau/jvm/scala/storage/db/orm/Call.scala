package ru.spbau.jvm.scala.storage.db.orm

import java.io.File
import java.time.LocalDateTime

import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.util.Date

import scala.io.Source

case class Call(id: Long, from: String, to: String, timestamp: LocalDateTime, duration: Long, cost: Double) {
  def this(row: Row) = {
    this(row.next().toLong, row.next(), row.next(), Date.fromDateTime(row.next()), row.next().toLong, row.next().toDouble)
  }
}

object Call {
  def load(file: File): Iterable[Call] = {
    val source = Source.fromFile(file)
    try {
      source.getLines.map((it: String) => new Call(new Row(it))).toList
    } finally {
      source.close()
    }
  }
}
