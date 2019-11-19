package ru.spbau.jvm.scala

import java.io.{FileNotFoundException, IOException}
import java.text.SimpleDateFormat

import scala.io.Source

object Main {
  private val datePattern = "\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d"
  private val helpPath = "resources/help.txt"
  private val numberRegex = "^number ([a-zA-Z]+) ([a-zA-Z]+)$".r
  private val callsRegex = ("^calls from (" + datePattern + ") " +
    "to (" + datePattern + ")$").r
  private val totalRegex = ("^total from (" + datePattern + ")" +
    " to (" + datePattern + ")$").r
  private val callsCostFromRegex = "^calls cost from ([\\d]+(.[\\d]+)?)$".r
  private val callsDurationFromRegex = "^calls duration from ([\\d]+)$".r

  def main(args: Array[String]): Unit = {
    try {
      Database.load()
    } catch {
      case _: FileNotFoundException | _: IOException =>
        println("Something went wrong while reading database files")
        return
    }
    while (true) {
      val command = scala.io.StdIn.readLine("enter command:\n")
      command match {
        case "avg" => println(Database.getAverageDuration + "s")
        case "max spender" => printUser(Database.getMaxSpender)
        case callsCostFromRegex(cost, _) =>
          printCalls(Database.getCallsWithCostMoreOrEqualTo(cost.toDouble))
        case callsDurationFromRegex(duration) =>
          printCalls(Database.getCallsWithDurationMoreOrEqualTo(duration.toInt))
        case numberRegex(firstName, lastName) =>
          getNumberByUser(firstName, lastName)
        case callsRegex(date1, date2) =>
          printCalls(Database.getCallsBetweenDates(dateFromString(date1), dateFromString(date2)))
        case totalRegex(date1, date2) =>
          val total = Database.getTotalBetweenDates(dateFromString(date1), dateFromString(date2))
          println(s"$$$total")
        case "help" =>
          val help = Source.fromFile(helpPath)
          help.getLines().foreach(println)
          help.close
        case "stop" => return
        case x => println(s"command $x not found")
      }
    }
  }

  private def printUser(user: User): Unit = {
    println(user.firstName + " " + user.lastName)
  }

  private def printCall(call: Call): Unit = {
    println(call.user.firstName + " " + call.user.lastName + " " + call.callee + " " + call.duration + " " + call.cost)
  }

  private def printCalls(calls: List[Call]): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    calls.foreach(printCall)
  }

  private def getNumberByUser(firstName : String, lastName : String): Unit = {
    val number = Database.getNumberByName(firstName, lastName)
    number match {
      case None => println("user " + firstName + " " + lastName + " does not exist")
      case Some(value) => println(value)
    }
  }

  private def dateFromString(dateString : String) = new SimpleDateFormat("dd.MM.yyyy").parse(dateString)

}