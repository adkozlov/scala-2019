package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.db.DB

import scala.io.StdIn

class CLI(db: DB) {
  private val invoker = new Invoker(db)

  def exec: Unit = {
    var commandName: String = null
    do {
      val args = StdIn.readLine().split("[\\s\\t]+")
      commandName = args(0)
      if (commandName != "exit") {
        println("\n" + invoker.exec(commandName, args.drop(1)))
      }
    } while (!commandName.equals("exit"))

//    println(db.pool.getActionTable().select("PhoneID", (x: String) => {x.toInt > 5}).getStr())
//    println(db.data.getActionTable().filterCols(List("Callee", "Start")).getStr())
//    println(db.usedPhone.getActionTable().joinBy(db.employee.getActionTable(), "EmployeeID").getStr())
  }
}
