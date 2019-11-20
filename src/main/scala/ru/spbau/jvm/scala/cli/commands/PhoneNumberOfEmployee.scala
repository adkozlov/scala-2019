package ru.spbau.jvm.scala.cli.commands

import ru.spbau.jvm.scala.db.DB

class PhoneNumberOfEmployee extends Command {
  override def exec(db: DB, args: Array[String]): String = {
    try {
      db.employee.getActionTable()
        .select("Name", x => x.equals(args(0)))
        .select("LastName", x => x.equals(args(1)))
        .joinBy(db.usedPhone.getActionTable(), "EmployeeID")
        .joinBy(db.pool.getActionTable(), "PhoneID")
        .filterCols(List("Name", "LastName", "PhoneNumber"), x => x)
        .getStr()
    } catch {
      case e: IllegalArgumentException =>
        "Invalid PhoneOfEmployee args: " + e.getMessage
    }
  }

  override def getInfo(): String = {
    "Phone number of employee by his name and lastname"
  }
}
