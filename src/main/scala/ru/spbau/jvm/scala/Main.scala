package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

import scala.io.StdIn
import scala.util.matching.Regex

object Main {
  private val database = BillingDatabase

  private def processCallsBetweenDates(fromDate: Date, toDate: Date): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
    database.getCallsBetweenDates(fromDate, toDate).foreach(call => println(call.toStringWithoutDate))
  }

  private def processAverageCallDuration(): Unit = println(s"${database.getAverageCallDuration}s")

  private def processTotalCallCostBetweenDates(fromDate: Date, toDate: Date): Unit =
    println(s"$$${database.getTotalCallCostBetweenDates(fromDate, toDate)}")

  private def processEmployeePhone(firstName: String, lastName: String): Unit = {
    try {
      println(database.getEmployeePhone(firstName, lastName))
    } catch {
      case e: IllegalArgumentException => println(e.getMessage)
    }
  }

  private def processTotalCostForEachEmployee(): Unit = {
    database.getTotalCostForEachEmployee.foreach {
      case (employee: Employee, cost: Double) => println(s"$employee: $$$cost")
    }
  }

  private def processCallsByEmployee(firstName: String, lastName: String): Unit = {
    println("Callee| Date | Duration (s) | Cost ($)")
    database.getCallsByEmployee(firstName, lastName).foreach(call => println(call.toStringWithoutEmployee))
  }

  private def processEmployeeWithHighestTotalCost(): Unit = {
    val employeeWithCost = database.getEmployeeWithHighestTotalCost
    if (employeeWithCost.isEmpty)
      println("There are no calls yet")
    else
      println(s"${employeeWithCost.get._1}: $$${employeeWithCost.get._2}")
  }

  private def processHelp(): Unit = {
    println("Phone calls billing")
    println()
    println("Поддерживаемые запросы:")
    println()
    println("calls from DATETIME to DATETIME - список всех звонков за заданный промежуток времени")
    println("total from DATETIME to DATETIME - суммарная стоимость услуг связи за заданный промежуток времени")
    println("avg - средняя длительность звонка")
    println("number VARCHAR VARCHAR - номер телефона заданного сотрудника")
    println("total for each employee - список сотрудников, совершивших хотя бы один звонок, с общей стоимостью всех звонков")
    println("calls by VARCHAR VARCHAR - список звонков от сотрудника")
    println("employee with highest total cost - сотрудник, на телефоне которого наибольший суммарный счет")
    println("help - вызов этой справки")
    println("exit - выход")
  }

  private val DateFormat = new SimpleDateFormat("dd.MM.yyyy")

  val DatePattern = """(\d\d\.\d\d.\d\d\d\d)"""
  val NamePattern = """(\w*)"""

  val CallsBetweenDates: Regex = s"""calls from $DatePattern to $DatePattern""".r
  val AverageCallDuration: Regex = """avg""".r
  val TotalCallCostBetweenDates: Regex = s"""total from $DatePattern to $DatePattern""".r
  val EmployeePhone: Regex = s"""number $NamePattern $NamePattern""".r
  val TotalCostForEachEmployee: Regex = s"""total for each employee""".r
  val CallsByEmployee: Regex = s"""calls by $NamePattern $NamePattern""".r
  val EmployeeWithHighestTotalCost: Regex = s"""employee with highest total cost""".r
  val Help: Regex = """help""".r
  val Exit: Regex = """exit""".r

  private def processQuery(query: String): Boolean = {
    query match {
      case CallsBetweenDates(from, to) =>
        processCallsBetweenDates(DateFormat.parse(from), DateFormat.parse(to))
      case AverageCallDuration() =>
        processAverageCallDuration()
      case TotalCallCostBetweenDates(from, to) =>
        processTotalCallCostBetweenDates(DateFormat.parse(from), DateFormat.parse(to))
      case EmployeePhone(firstName, lastName) =>
        processEmployeePhone(firstName, lastName)
      case TotalCostForEachEmployee() =>
        processTotalCostForEachEmployee()
      case CallsByEmployee(firstName, lastName) =>
        processCallsByEmployee(firstName, lastName)
      case EmployeeWithHighestTotalCost() =>
        processEmployeeWithHighestTotalCost()
      case Help() =>
        processHelp()
      case Exit() =>
        return false
      case _ =>
        println(s"command '$query' not found")
    }
    true
  }

  def main(args: Array[String]): Unit = {
    database.loadDatabase()

    var continue = true
    do {
      print("$ ")
      val query = StdIn.readLine()
      continue = processQuery(query)
    } while (continue)
  }
}