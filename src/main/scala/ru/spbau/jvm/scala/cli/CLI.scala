package ru.spbau.jvm.scala.cli

import ru.spbau.jvm.scala.cli.commands._
import ru.spbau.jvm.scala.db.BillingDB

import scala.collection.mutable.ListBuffer
import scala.io.StdIn

class CLI(db: BillingDB) {

  private val commands = ListBuffer(new NumberCommand, new TotalCommand, new AvgCommand, new CallsCommand)
  val help = new HelpCommand(commands)
  commands += help

  private def help(args: Array[String]): (String, Iterable[String]) = {
    ("help", commands.map(_.info()))
  }

  def start(): Unit = {
    while (true) {
      var input: String = null
      var splittedInput: Array[String] = null
      do {
        input = StdIn.readLine()
        splittedInput = input.split("[\\t\\s]+")
      } while (splittedInput.isEmpty)
      val command = commands.find(command => command.name() == splittedInput(0)).getOrElse(new UnknownCommand)
      val (header: String, result: Iterable[String]) = command.execute(db, splittedInput)
      println(header)
      if (result.nonEmpty) {
        println(result.mkString("\n"))
      }
    }
  }
}
