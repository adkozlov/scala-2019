package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.cli.commands.{AvgCallDuration, CallsFromDateTimeRange, Command, PhoneNumberOfEmployee}
import ru.spbau.jvm.scala.db.DB

import scala.io.StdIn

class CLI(db: DB) {
  private val nameToCommand: Map[String, _ <: Command] =
    Map("CallsFromDateTimeRange" -> new CallsFromDateTimeRange,
      "AvgCallDuration" -> new AvgCallDuration,
      "PhoneNumberOfEmployee" -> new PhoneNumberOfEmployee)

  private val invoker = new Invoker(db)

  def exec: Unit = {
    nameToCommand.foreach(info => invoker.register(info._1, info._2))

    while (true) {
      val args = StdIn.readLine().split("[\\s\\t]+")
      if (args.nonEmpty) {
        args(0) match {
          case "Exit" =>
            return
          case "Help" =>
            println("\n" + nameToCommand.map((x: (String, Command)) => {
              x._1 + " : " + x._2.getInfo()
            }).toList.mkString("\n"))
          case _ =>
            println("\n" + invoker.exec(args(0), args.drop(1)))
        }
      }
    }
  }
}
