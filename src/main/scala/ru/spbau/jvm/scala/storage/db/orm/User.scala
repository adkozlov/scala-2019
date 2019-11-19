package ru.spbau.jvm.scala.storage.db.orm

import java.io.File

import ru.spbau.jvm.scala.storage.db.Row

import scala.io.Source

case class User(id: Long, firstName: String, lastName: String) {
  def this(row: Row) = {
    this(row.next().toLong, row.next(), row.next())
  }
}

object User {
  def load(file: File): Iterable[User] = {
    val source = Source.fromFile(file)
    try {
      source.getLines.map((it: String) => new User(new Row(it))).toList
    } finally {
      source.close()
    }
  }
}
