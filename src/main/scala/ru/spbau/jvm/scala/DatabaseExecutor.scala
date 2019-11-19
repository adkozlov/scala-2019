package ru.spbau.jvm.scala

import java.io.{OutputStreamWriter, Writer}

import scala.io.StdIn

object DatabaseExecutor {
  private val totalPattern = ("^total( from ([0-9]{2})\\.([0-9]{2})\\.([0-9]{4})" +
                              "( to ([0-9]{2})\\.([0-9]{2})\\.([0-9]{4}))?)?$").r
  private val callsPattern = ("^calls( from ([0-9]{2})\\.([0-9]{2})\\.([0-9]{4})" +
                              "( to ([0-9]{2})\\.([0-9]{2})\\.([0-9]{4}))?)?$").r
  private val avgPattern = "^avg$".r
  private val numberPattern = "^number ([\\S]+) ([\\S]+)$".r
  private val helpPattern = "^help$".r
  private val sortUsersPattern = "^sortUsers$".r
  private val lastCallPattern = "^lastCall ([\\S]+) ([\\S]+)$".r
  private val freeNumbersPattern = "^freeNumbers$".r
  private val exitPattern = "^exit$".r

  private val help =
    """help -- shows this message
      |total[ from <DD.MM.YYYY>[ to <DD.MM.YY>]] -- shows total cost of all operations in given period
      |avg -- shows average call duration
      |calls[ from <DD.MM.YYYY>[ to <DD.MM.YY>]] -- shows list of all calls in given period
      |number <NAME> <SURNAME> -- shows number of given user
      |sortUsers -- shows list of all users ordered by decrease of their outlay
      |lastCall <NAME> <SURNAME> -- shows day when given user called last time
      |freeNumbers -- shows list of numbers from the pool that are free
      |exit -- exits program
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val database = new Database()
    val writer = new OutputStreamWriter(System.out)
    while (true) {
      val input = StdIn.readLine()
      input match {
        case exitPattern() =>
          return
        case _ =>
          execute(database, input, writer)
      }
    }
  }

  def execute(database: Database, input: String, writer: Writer): Boolean = {
    input match {
      case totalPattern(null, null, null, null, null, null, null, null) =>
        executeTotal(database, writer, DateRange())
      case totalPattern(_, day1, month1, year1, null, null, null, null) =>
        val date = Date(day1.toInt, month1.toInt, year1.toInt)
        executeTotal(database, writer, DateRange(date))
      case totalPattern(_, day1, month1, year1, _, day2, month2, year2) =>
        val date1 = Date(day1.toInt, month1.toInt, year1.toInt)
        val date2 = Date(day2.toInt, month2.toInt, year2.toInt)
        executeTotal(database, writer, DateRange(date1, date2))
      case callsPattern(null, null, null, null, null, null, null, null) =>
        executeCalls(database, writer, DateRange())
      case callsPattern(_, day1, month1, year1, null, null, null, null) =>
        val date = Date(day1.toInt, month1.toInt, year1.toInt)
        executeCalls(database, writer, DateRange(date))
      case callsPattern(_, day1, month1, year1, _, day2, month2, year2) =>
        val date1 = Date(day1.toInt, month1.toInt, year1.toInt)
        val date2 = Date(day2.toInt, month2.toInt, year2.toInt)
        executeCalls(database, writer, DateRange(date1, date2))
      case helpPattern() =>
        writer.write(help)
      case numberPattern(firstName, lastName) =>
        val phones = database.number(firstName, lastName)
        if (phones.isEmpty) {
          writer.write(s"employee '$firstName $lastName' not found\n")
        } else {
          for (phone <- phones) {
            writer.write(s"$phone\n")
          }
        }
      case avgPattern() =>
        writer.write(s"${database.average().toString}\n")
      case lastCallPattern(firstName, lastName) =>
        val result = database.lastCall(firstName, lastName)
        writer.write(s"${result.getOrElse(s"No calls from \'$firstName $lastName\'")}\n")
      case freeNumbersPattern() =>
        for (number <- database.freeNumbers()) {
          writer.write(s"$number\n")
        }
      case sortUsersPattern() =>
        val sorted = database.sortUsers()
        writer.write("FirstName | LastName | Outlay ($)\n")
        for ((firstName, lastName, outlay) <- sorted) {
          writer.write(s"$firstName | $lastName | $outlay\n")
        }
      case _ =>
        writer.write(s"command \'$input\' not found\n")
    }
    writer.flush()
    false
  }

  private def executeTotal(database: Database, writer: Writer, range: DateRange): Unit = {
    writer.write(s"${database.total(range)}\n")
  }

  private def executeCalls(database: Database, writer: Writer, range: DateRange): Unit = {
    writer.write("FirstName | LastName | Callee | Duration (s) | Cost ($)\n")
    for (callResult <- database.calls(range)) {
      writer.write(s"${callResult.person.firstName} | ${callResult.person.lastName} | " +
        s"${callResult.callee} | ${callResult.duration} | ${callResult.cost}\n")
    }
  }
}
