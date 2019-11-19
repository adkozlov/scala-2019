package ru.spbau.jvm.scala

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import ru.spbau.jvm.scala.CallsDB.{Call, Name}

import scala.io.StdIn
import scala.util.{Success, Try}

object Main {
  private val fromToSubPattern = "(?:\\s+from ([0-9.tT:\\-]+))?(?:\\s+to ([0-9.tT:\\-]+))?"
  private val nameSubPattern = "\\s+(\\p{L}+)\\s+(\\p{L}+)"
  private val numberSubPattern = "\\s+(\\+(?:\\s*[0-9])+)"

  private val callsPattern = s"calls$fromToSubPattern".r
  private val avgPattern = s"avg$fromToSubPattern".r
  private val totalPattern = s"total$fromToSubPattern".r
  private val numberPattern = s"number$nameSubPattern".r
  private val employeePattern = s"employee$numberSubPattern".r
  private val outgoingPattern = s"outgoing$nameSubPattern$fromToSubPattern".r
  private val incomingPattern = s"incoming$nameSubPattern$fromToSubPattern".r
  private val helpPattern = "help".r

  private val helpFromTo = "[from DATETIME] [to DATETIME]"
  private val helpName = "FIRST_NAME LAST_NAME"
  private val helpMessage =
    s"""
       |calls $helpFromTo
       | print all calls which were made during the specified period
       |
       |avg $helpFromTo
       | print the average duration of calls which were made during the specified period
       |
       |total $helpFromTo
       | print the total cost of calls which were made during the specified period
       |
       |number $helpName
       | print the number that was assigned to the specified employee
       |
       |employee PHONE_NUMBER
       | print the name of the employee that uses the specified phone number
       |
       |outgoing $helpName $helpFromTo
       | print all calls which were made by the specified employee during the specified period
       |
       |incoming $helpName $helpFromTo
       | print all calls which were received by the specified employee during the specified period
       |
       |help
       | show help
       |""".stripMargin

  private val cannotFindEmployee = "Can't find the specified employee"

  def main(args: Array[String]): Unit = {
    val callsDb = CallsDB()

    var shouldContinue = true
    while (shouldContinue) {
      val lineOption = Option(StdIn.readLine()).map(_.trim)

      val linesToPrint: Option[Seq[String]] = lineOption.map {
        case callsPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          callsDb.getCalls(timeFrom, timeTo)
            .map(callToString)
        }
        case avgPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          Seq(callsDb.getAvg(timeFrom, timeTo).toString)
        }
        case totalPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          Seq(callsDb.getTotal(timeFrom, timeTo).toString)
        }
        case numberPattern(firstName, lastName) => Seq(
          Try(callsDb.getNumber(Name(firstName, lastName)))
            .getOrElse(cannotFindEmployee)
        )
        case employeePattern(number) => Try(callsDb.getEmployee(number.trim))
          .map(_.map(name => s"${name.firstName} ${name.lastName}"))
          .getOrElse(Some("This phone number is not owned by the Company"))
          .orElse(Some("No employee is using that phone number"))
          .toSeq
        case outgoingPattern(firstName, lastName, from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          val name = Name(firstName, lastName)
          Try(callsDb.getOutgoing(name, timeFrom, timeTo))
            .map(_.map(callToString))
            .getOrElse(Seq(cannotFindEmployee))
        }
        case incomingPattern(firstName, lastName, from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          val name = Name(firstName, lastName)
          Try(callsDb.getIncoming(name, timeFrom, timeTo))
            .map(_.map(callToString))
            .getOrElse(Seq(cannotFindEmployee))
        }
        case helpPattern() => Seq(helpMessage)
        case other => Seq(s"command '$other' not found")
      }

      linesToPrint.toSeq.flatten.foreach(println)
      shouldContinue = lineOption.isDefined
    }
  }

  private def executeFunctionWithTime(from: String, to: String)
                                     (function: (LocalDateTime, LocalDateTime) => Seq[String]): Seq[String] = {
    val (optionFrom, optionTo) = (Option(from), Option(to))
    val tryFrom: Try[LocalDateTime] = optionFrom.map(parseAsLocalDateTime).getOrElse(Success(LocalDateTime.MIN))
    val tryTo: Try[LocalDateTime] = optionTo.map(parseAsLocalDateTime).getOrElse(Success(LocalDateTime.MAX))
    (tryFrom, tryTo) match {
      case (Success(timeFrom), Success(timeTo)) => function(timeFrom, timeTo)
      case _ => Seq("could not parse time arguments")
    }
  }

  private def parseAsLocalDateTime(time: String): Try[LocalDateTime] = {
    Try(LocalDateTime.parse(time)).orElse {
      Try(LocalDate.parse(time)).map(_.atStartOfDay())
    }.orElse {
      Try(LocalDate.parse(time, DateTimeFormatter.ofPattern("dd.MM.yyyy"))).map(_.atStartOfDay())
    }
  }

  private def callToString(call: Call): String = {
    val Call(caller, callee, duration, cost) = call
    s"${caller.firstName} | ${caller.lastName} | ${formatNumber(callee)} | $duration | $cost"
  }

  private def formatNumber(number: String): String = {
    number.replaceFirst("(\\d{2})(\\d{4})", "$1 $2 ")
  }
}
