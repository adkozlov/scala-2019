package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.io.StdIn

object Main {

  final val CALLS_FILE_NAME = "resources/calls.txt"
  final val EMPLOYEES_FILE_NAME = "resources/employees.txt"
  final val datePattern = DateTimeFormat.forPattern("dd.MM.yyyy")

  def main(args: Array[String]): Unit = {
    val database = new Database(callsFileName = CALLS_FILE_NAME,
                                employeesFileName = EMPLOYEES_FILE_NAME)

    val dateRegEx = "[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}".r

    val calls = s"calls from ($dateRegEx) to ($dateRegEx)".r
    val average = "avg".r
    val totalCostFromTo = s"total from ($dateRegEx) to ($dateRegEx)".r
    val totalCostFrom = s"total from ($dateRegEx)".r
    val totalCost = "total".r
    val numberByName = "number ([a-zA-Z]+) ([a-zA-Z]+)".r
    val help = "help".r
    val employeeCalls = "calls from ([a-zA-Z]+) ([a-zA-Z]+)".r
    val groupCalls = s"group calls from ($dateRegEx) to ($dateRegEx)".r
    val employees = "employees".r

    val helpInfo = """
        |calls from DATETIME to DATETIME - prints a filtered by time calls list
        |avg - prints average call duration
        |total( from DATETIME( to DATETIME)?)? - prints total cost of calls in time period if specified
        |number VARCHAR VARCHAR - prints employee's phone number
        |calls from VARCHAR VARCHAR - prints all data about calls from employee
        |help - prints this message
        |group calls from DATETIME to DATETIME - total cost and duration for each employee
        |employees -- print all employees and phone numbers
        |""".stripMargin

    while (true) {
      val command = StdIn.readLine()
      try {
        command match {
          case calls(from, to) =>
            println("FirstName | LastName | Callee | Duration (s) | Cost ($)")
            database.calls(toDate(from), toDate(to))
              .foreach(call =>
                println(s"${call.employee.firstName}|${call.employee.lastName}|${call.callee}|${call.duration}|${call.cost}")
              )
          case average() => println(s"${database.averageCallDuration()}s")
          case totalCostFromTo(from, to) => println(round(database.totalCost(toDate(from), toDate(to)), 2))
          case totalCostFrom(from) => println(round(database.totalCost(toDate(from)), 2))
          case totalCost() => println(round(database.totalCost(), 2))
          case numberByName(firstName, lastName) => database.getPhone(Employee(firstName, lastName)) match {
              case Some(number) => println(number)
              case None => println(s"employee '$firstName $lastName' not found")
            }
          case help() => println(helpInfo)
          case employeeCalls(firstName, lastName) =>
            database.callsFromEmployee(Employee(firstName, lastName)) match {
              case Some(calls) =>
                println("Callee | Duration (s) | Cost ($) | Date")
                calls.foreach(call => println(s"${call.callee}|${call.duration}|${call.cost}|${call.date}"))
              case None => println(s"employee '$firstName $lastName' not found")
            }
          case groupCalls(from, to) =>
            println("FirstName | LastName | Duration (s) | Cost ($)")
            database.groupCallsByEmployee(toDate(from), toDate(to))
              .foreach(call =>
                println(s"${call.employee.firstName}|${call.employee.lastName}|${call.duration}|${call.cost}")
              )
          case employees() =>
            println("FirstName | LastName | Phone")
            database.getEmployees.foreach(p => println(s"${p._1.firstName}|${p._1.lastName}|${p._2}"))
          case _ => println(s"command '$command' not found")
        }
      } catch {
        case _: org.joda.time.IllegalFieldValueException => println("Illegal date format. Please use format 'dd.MM.yyyy'.")
        case e: Throwable =>
          e.printStackTrace()
          return
      }
    }
  }

  def round(x: Double, k: Int): Double = {
    val m = Math.pow(10, k)
    Math.round(x * m) / m
  }

  def toDate(s: String): DateTime = DateTime.parse(s, datePattern)
}
