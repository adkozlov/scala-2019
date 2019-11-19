package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.DataRow

class Data extends Table[DataRow] {
  override def load(file: File): Unit  = {
    super.loadWithRowFabric(file, DataRow)
  }
}
