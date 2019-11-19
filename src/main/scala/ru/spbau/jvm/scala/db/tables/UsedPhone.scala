package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.UsedPhoneRow

class UsedPhone extends Table[UsedPhoneRow] {
  override def load(file: File): Unit  = {
    super.loadWithRowFabric(file, UsedPhoneRow)
  }
}
