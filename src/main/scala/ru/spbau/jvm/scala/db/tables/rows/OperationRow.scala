package ru.spbau.jvm.scala.db.tables.rows

object OperationRow extends RowFabric {
  override def create(cols: Array[String]): OperationRow = {
    cols match {
      case Array(id: String, name: String, cost: String) =>
        try {
          OperationRow(id.toInt, name, cost.toFloat)
        } catch {
          case _:  NumberFormatException  =>
            throw new IllegalArgumentException("Invalid operation data: using not a number")
        }
      case _ =>
        throw new IllegalArgumentException("Invalid operation data")
    }
  }
}

case class OperationRow(id: Int, name: String, cost: Float) extends Row {
  data = List(id, name, cost)
}
