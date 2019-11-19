package ru.spbau.jvm.scala.db.tables.rows

import org.joda.time.DateTime

object UsedPhoneRow extends RowFabric {
  override def create(cols: Array[String]): UsedPhoneRow = {
    cols match {
      case Array(idEmp: String, idPhone: String, start: String, isAlive: String) =>
        try {
          UsedPhoneRow(idEmp.toInt,
            idPhone.toInt,
            DateTime.parse(start, DATE_TIME_PATTERN),
            isAlive.toBoolean)
        } catch {
          case _:  NumberFormatException =>
            throw new IllegalArgumentException("Invalid usedPhone data: using not a number")
          case e: IllegalArgumentException =>
            throw new IllegalArgumentException("Invalid usedPhone data: " + e.getMessage)
        }
      case _ =>
        throw new IllegalArgumentException("Invalid usedPhone data")
    }
  }
}

case class UsedPhoneRow(idEmp: Int, idPhone: Int, start: DateTime, isAlive: Boolean) extends Row {
  data = List(idEmp, idPhone, start, isAlive)
}
