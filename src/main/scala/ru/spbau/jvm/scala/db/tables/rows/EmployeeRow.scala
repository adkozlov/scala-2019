package ru.spbau.jvm.scala.db.tables.rows

object EmployeeRow extends RowFabric {
  override def create(cols: Array[String]): EmployeeRow = {
    cols match {
      case Array(id: String, name: String, lastName: String) =>
        try {
          EmployeeRow(id.toInt, name, lastName)
        } catch {
          case _:  NumberFormatException  =>
            throw new IllegalArgumentException("Invalid employee data: using not a number")
        }
      case _ =>
        throw new IllegalArgumentException("Invalid employee data")
    }
  }
}

case class EmployeeRow(id: Int, name: String, lastName: String) extends Row {
  data = List(id, name, lastName)
}
