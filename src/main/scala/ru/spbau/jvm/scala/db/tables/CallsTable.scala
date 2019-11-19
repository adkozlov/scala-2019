package ru.spbau.jvm.scala.db.tables

import ru.spbau.jvm.scala.db.rows.CallsRow


class CallsTable extends Table[CallsRow] {
  override def parseRow(line: String): CallsRow = {
    line.split(",") match {
      case Array(id: String, phoneNumberFrom: String, phoneNumberTo: String, dateTime: String,
      duration: String, cost: String) =>
        CallsRow(id, phoneNumberFrom, phoneNumberTo, dateTime, duration.toDouble, cost.toDouble)
      case _ => throw new IllegalArgumentException("Invalid call line")
    }
  }
}
