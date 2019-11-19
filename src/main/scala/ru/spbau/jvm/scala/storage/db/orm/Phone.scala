package ru.spbau.jvm.scala.storage.db.orm

import java.io.File

import ru.spbau.jvm.scala.storage.db.Row

import scala.io.Source

case class Phone(id: Long, userId: Long, phoneNumber: String) {
  def this(row: Row) = {
    this(row.next().toLong, row.next().toLong, row.next())
  }
}


object Phone {
  def load(file: File): Iterable[Phone] = {
    val source = Source.fromFile(file)
    try {
      source.getLines.map((it: String) => new Phone(new Row(it))).toList
    } finally {
      source.close()
    }
  }
}
