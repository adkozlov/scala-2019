package ru.spbau.jvm.scala.db.tables.rows

import org.joda.time.DateTime

object DataRow extends RowFabric {
  override def create(cols: Array[String]): DataRow = {
    cols match {
      case Array(id: String, phone: String, opType: String, start: String, finish: String) =>
        try {
          DataRow(id.toInt,
            phone,
            opType.toInt,
            DateTime.parse(start),
            DateTime.parse(finish))
        } catch {
          case _: NumberFormatException =>
            throw new IllegalArgumentException("Invalid data data: using not a number")
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException("Invalid usedPhone data: " + e.getMessage)
        }
      case _ =>
        throw new IllegalArgumentException("Invalid data data")
    }
  }
}

case class DataRow(id: Int, phone: String, opType: Int, start: DateTime, finish: DateTime) extends Row {
  data = List(id, phone, opType, start, finish)
}
