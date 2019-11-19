package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.OperationRow

class Operation extends Table[OperationRow] {
  override def load(file: File): Unit  = {
    super.loadWithRowFabric(file, OperationRow)
  }
}
