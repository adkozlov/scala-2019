package ru.spbau.jvm.scala.db.tables

import ru.spbau.jvm.scala.db.rows.{PhoneRow, UserRow}

class PhonesTable extends Table[PhoneRow] {
  override def parseRow(line: String): PhoneRow = {
    line.split(",") match {
      case Array(id: String, userId: String, phoneNumber: String) =>
        PhoneRow(id, userId, phoneNumber)
      case _ => throw new IllegalArgumentException("Invalid phone line")
    }
  }
}
