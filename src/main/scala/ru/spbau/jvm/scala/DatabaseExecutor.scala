package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.StdIn
import scala.util.matching.Regex

class DatabaseExecutor(private val database: Database) {

  private val dateRegex = "\\d\\d\\.\\d\\d\\.\\d\\d\\d\\d".r
  private val nameRegex = "[\\w]+".r
  private val phoneRegex = "[+]?[\\d]+".r

  private val callsFromTo: Regex = s"""calls from ($dateRegex) to ($dateRegex)""".r
  private val callsFrom: Regex = s"""calls from ($dateRegex)""".r
  private val calls: Regex = """calls""".r
  private val callsAverageDuration: Regex = """avg""".r
  private val costFromTo: Regex = s"""total from ($dateRegex) to ($dateRegex)""".r
  private val costFrom: Regex = s"""total from ($dateRegex)""".r
  private val cost: Regex = """total""".r
  private val phoneFromUser: Regex = s"""number ($nameRegex) ($nameRegex)""".r
  private val userFromPhone: Regex = s"""user ($phoneRegex)""".r
  private val maxDuration: Regex = """max duration""".r
  private val minDuration: Regex = """min duration""".r
  private val help: Regex = """help""".r
  private val exit: Regex = """exit""".r

  private val helpMessage = """help - shows this message
               |calls[ from DATETIME]? to DATETIME]?]? - shows calls in period (DATETIME?, DATETIME?)
               |avg - shows average call duration
               |total[ from DATETIME[ to DATETIME]?]? - shows total cost of calls in period (DATETIME?, DATETIME?)
               |number VARCHAR VARCHAR - shows user's phone number
               |user VARCHAR - shows phone's number user
               |max duration -- shows maximal duration of calls
               |min duration -- shows minimal duration of calls
               |""".stripMargin

  def run(): Unit = {
    var execute: Boolean = true

    try {
      database.load()
    } catch {
      case e: DatabaseException => {
        print(e.getMessage)
        execute = false
      }
    }
    while (execute) {
      val query = StdIn.readLine()
      execute = executeQuery(query)
    }
  }

  private def executeQuery(query: String): Boolean = {
    query match {
      case callsFromTo(from, to) =>
        printListResult(database.getCallsInPeriod(toDate(from), toDate(to)))

      case callsFrom(from) =>
        printListResult(database.getCallsInPeriod(toDate(from)))

      case calls() =>
        printListResult(database.getCallsInPeriod())

      case callsAverageDuration() =>
        println(s"${database.getCallsAverageDuration}")

      case costFromTo(from, to) =>
        println(s"${database.getCostInPeriod(toDate(from), toDate(to))}")

      case costFrom(from) =>
        println(s"${database.getCostInPeriod(toDate(from))}")

      case cost() =>
        println(s"${database.getCostInPeriod()}")

      case phoneFromUser(name, surname) =>
        val result = database.getPhoneFromUser(name, surname)
        if (result.isEmpty) {
          println(s"No phone for user $name $surname")
        } else {
          println(s"${result.get}")
        }

      case userFromPhone(phone) =>
        val result = database.getUserFromPhone(phone)
        if (result.isEmpty) {
          println(s"No user for phone $phone")
        } else {
          println(s"${result.get._1} ${result.get._2}")
        }

      case maxDuration() =>
        val result = database.getMaxDuration
        if (result.isEmpty) {
          println(s"Calls database is empty")
        } else {
          println(result.get)
        }

      case minDuration() =>
        val result = database.getMinDuration
        if (result.isEmpty) {
          println(s"Calls database is empty")
        } else {
          println(result.get)
        }

      case help() => print(helpMessage)

      case exit() => return false

      case _ => println(s"Query '$query' not found!")
    }

    true
  }

  private def toDate(date: String): DateTime = DateTime.parse(date, DateTimeFormat.forPattern("dd.MM.yyyy"))

  private def printListResult(result: List[(String, String, String, Int, Double)]): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    for ((name, surname, phone, duration, cost) <- result) {
      println(s"$name | $surname | $phone | $duration | $cost")
    }
  }
}
