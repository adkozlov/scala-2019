package ru.spbau.jvm.scala.db.tables.rows

object PoolRow extends RowFabric {
  override def create(cols: Array[String]): PoolRow = {
    cols match {
      case Array(id: String, phone: String) =>
        try {
          PoolRow(id.toInt, phone)
        } catch {
          case _:  NumberFormatException =>
            throw new IllegalArgumentException("Invalid pool data: using not a number")
        }
      case _ =>
        throw new IllegalArgumentException("Invalid pool data")
    }
  }
}

case class PoolRow(id: Int, phone: String) extends Row {
  data = List(id, phone)
}
