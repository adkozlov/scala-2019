package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.db.DB

class TableActionTests extends FlatSpec with Matchers {
  val db = new DB()
  db.loadTables("resources")

  "pool:" should "PhoneID > 7" in {
    val res: String = db.pool.getActionTable().select("PhoneID", (x: String) => {x.toInt > 7}).getStr()
    res should be(List(
      "PhoneID | PhoneNumber",
      "8 | 89202452624",
      "9 | 83416555098").mkString("\n"))
  }

  "data, operation:" should "{PhoneID | Callee | Description}" in {
    val res: String = db.data.getActionTable().filterCols(List("Start", "Finish"), x => !x)
      .select("OperationType", (x: String) => {x.toInt == 0 || x.toInt == 2})
      .joinBy(db.operation.getActionTable(), "OperationType")
      .filterCols(List("OperationType", "CostPerMinute($)"), x => !x)
      .getStr()
    res should be(List(
      "PhoneID | Callee | Description",
      "4 | 85974701195 | out_call",
      "7 | 85088708130 | out_call",
      "0 | 85088708130 | out_call",
      "4 | 84892417475 | out_sms",
      "5 | 86718740469 | out_sms").mkString("\n"))
  }

  "usedPhone, employee: " should "{EmployeeID | PhoneID | Name | LastName}" in {
    val res: String = db.usedPhone.getActionTable()
      .joinBy(db.employee.getActionTable(), "EmployeeID")
      .filterCols(List("UsingStart", "IsAlive"), x => !x)
      .select("PhoneID", (x: String) => {x.toInt < 3})
      .getStr()
    res should be(List("EmployeeID | PhoneID | Name | LastName",
      "2 | 1 | Sandy | Brummond",
      "4 | 2 | Steven | Wylie",
      "8 | 0 | Hortensia | Markowitz").mkString("\n"))
  }
}
