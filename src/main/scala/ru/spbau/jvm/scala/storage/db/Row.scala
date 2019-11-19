package ru.spbau.jvm.scala.storage.db

class Row(list: Iterable[String]) {
  private val iterator = list.iterator

  def this(line: String) = {
    this(line.split(",").map((it: String) => it.trim))
  }

  def next(): String = iterator.next()
}
