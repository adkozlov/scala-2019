package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.EmployeeRow

class Employee extends Table[EmployeeRow] {
  override def load(file: File): Unit  = {
    super.loadWithRowFabric(file, EmployeeRow)
  }
}
