package ru.spbau.jvm.scala.cli

import java.io.{FileNotFoundException, IOException}
import java.text.SimpleDateFormat
import java.util.Date

import ru.spbau.jvm.scala.BillingDatabase
import ru.spbau.jvm.scala.BillingDatabase.PhoneNumber
import ru.spbau.jvm.scala.cli.QueryPatterns._
import ru.spbau.jvm.scala.company_entity.User
import ru.spbau.jvm.scala.operator_entity.Call

import scala.io.{Source, StdIn}

object Main {
  private val HelpTextFile = "resources/help.txt"
  private val DateFormat = new SimpleDateFormat("dd.MM.yyyy")

  private val database = BillingDatabase

  private def parseDate(date: String): Date = DateFormat.parse(date)

  private def processListCalls(fromDate: Date, toDate: Date): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    database.getCallsBetweenDates(fromDate, toDate)
      .foreach { case (user: User, call: Call) =>
        println(
          s"${user.firstName} | ${user.lastName} | ${call.callee} | " +
            s"${call.duration} | ${call.cost}")
      }
  }

  private def processAverageDuration(): Unit =
    println(s"${database.getAverageDuration}s")

  private def processTotalCost(fromDate: Date, toDate: Date): Unit =
    println(s"$$${database.getTotalCostBetweenDates(fromDate, toDate)}")

  private def processEmployeePhone(firstName: String, lastName: String): Unit = {
    try {
      val phone = database.getPhoneByUser(User(firstName, lastName))
      if (phone.isEmpty) {
        println("User has no phone.")
      } else {
        println(phone.get)
      }
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
  }

  private def printUserMetric(metricResult: Option[(User, Double)]): Unit = {
    if (metricResult.isEmpty) {
      println("No calls have been done.")
    } else {
      println(s"User ${metricResult.get._1}: ${metricResult.get._2}")
    }
  }

  private def processMaxSpending(): Unit =
    printUserMetric(database.getUserMaxSpending)

  private def processMaxDuration(): Unit =
    printUserMetric(database.getUserWithMaxTotalDuration)

  private def processWhichPhone(phoneNumber: PhoneNumber): Unit = {
    try {
      val user = database.getUserByPhone(phoneNumber)
      if (user == null) {
        println(s"Number $phoneNumber is not attached to any user.")
      } else {
        println(user)
      }
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
  }

  private def showHelp(): Unit = {
    val phonesTable = Source.fromFile(HelpTextFile)
    phonesTable.getLines.foreach(println(_))
    phonesTable.close()
  }

  private def processQuery(query: String): Boolean = {
    query match {
      case QueryPatterns.ListCallsBetweenDates(fromDate, toDate) =>
        processListCalls(parseDate(fromDate), parseDate(toDate))

      case AverageCallDuration() =>
        processAverageDuration()

      case TotalCostBetweenDates(fromDate, toDate) =>
        processTotalCost(parseDate(fromDate), parseDate(toDate))

      case EmployeePhone(firstName, lastName) =>
        processEmployeePhone(firstName, lastName)

      case WhichPhone(phoneNumber) =>
        processWhichPhone(phoneNumber)

      case UserWithMaxSpending() =>
        processMaxSpending()

      case UserWithMaxDuration() =>
        processMaxDuration()

      case Help() => showHelp()

      case Exit() => return false

      case _ => println(s"command $query not found")
    }
    true
  }

  private def loadDatabase(): Unit = {
    try {
      database.loadDatabase()
    } catch {
      case e: FileNotFoundException =>
        println("One or more files of database cannot be found. ")
        println(e.printStackTrace())
      case e: IOException =>
        println("An error during the reading database.")
        println(e.printStackTrace())
      case e: IllegalStateException =>
        println("Invalid data found.")
        println(e.printStackTrace())
      case e: Any =>
        println("Something went wrong while extracting data from the database. " +
          "Perhaps, data format is invalid or corrupted.")
        println(e.printStackTrace())
    }
  }

  def main(args: Array[String]): Unit = {
    loadDatabase()

    var shouldContinue = true
    do {
      print("$ ")
      val query = StdIn.readLine()
      shouldContinue = processQuery(query)
    } while (shouldContinue)
  }
}
