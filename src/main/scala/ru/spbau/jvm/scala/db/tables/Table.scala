package ru.spbau.jvm.scala.db.tables

import ru.spbau.jvm.scala.db.rows.{Row, UserRow}

import scala.collection.mutable.ListBuffer
import scala.io.Source

trait Table[RowType <: Row] extends Iterable[RowType] {
  val content: ListBuffer[RowType] = ListBuffer[RowType]()

  override def iterator: Iterator[RowType] = {
    content.iterator
  }

  def +=(that: RowType): Unit = {
    content += that
  }

  def parseRow(line: String): RowType;

  def load(path: String): Unit = {
    val file = Source.fromFile(path)
    file.getLines().foreach { line =>
      val row: RowType = parseRow(line)
      this += row
    }
    file.close()
  }
}
