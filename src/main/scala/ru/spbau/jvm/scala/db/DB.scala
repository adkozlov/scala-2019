package ru.spbau.jvm.scala.db

import java.io.File

import ru.spbau.jvm.scala.db.tables.{Data, Employee, Operation, Pool, UsedPhone, Table}
import ru.spbau.jvm.scala.db.tables.rows.Row

class DB {
  var data: Data = new Data()
  var employee: Employee = new Employee()
  var operation: Operation = new Operation()
  var pool: Pool = new Pool()
  var usedPhone: UsedPhone = new UsedPhone()

  val tables: List[Table[_ <: Row]] = List(data, employee, operation, pool, usedPhone)
  val fileNames: List[String] = List("data.csv", "employee.csv", "operation.csv", "pool.csv", "used_phone.csv")

  def loadTables(pathDir: String): Unit = {
    fileNames.zip(tables).toMap.foreach(info => {
      val file = new File(pathDir + "/" + info._1)
      if (!file.exists()) {
        throw new IllegalArgumentException("Path " + info._1 + " is invalid")
      }
      info._2.load(file)
    })
  }
}
