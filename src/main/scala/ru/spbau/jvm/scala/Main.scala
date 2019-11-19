package ru.spbau.jvm.scala

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import ru.spbau.jvm.scala.CallsDB.{Call, Name}

import scala.io.StdIn
import scala.util.{Success, Try}

object Main {
  private val fromToSubPattern = "(?:\\s+from ([0-9.\\-]+))?(?:\\s+to ([0-9.\\-]+))?"
  private val nameSubPattern = "\\s+(\\p{L}+)\\s+(\\p{L}+)"
  private val numberSubPattern = "\\s+(\\+(?:\\s*[0-9])+)"

  private val callsPattern = s"calls$fromToSubPattern".r
  private val avgPattern = s"avg$fromToSubPattern".r
  private val totalPattern = s"total$fromToSubPattern".r
  private val numberPattern = s"number$nameSubPattern".r
  private val employeePattern = s"employee$numberSubPattern".r
  private val outgoingPattern = s"outgoing$nameSubPattern$numberSubPattern".r
  private val incomingPattern = s"incoming$nameSubPattern$numberSubPattern".r
  private val helpPattern = "help".r

  private val helpMessage =
    """
      |hello there!
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val callsDb = CallsDB()

    var shouldContinue = true
    while(shouldContinue) {
      val lineOption = Option(StdIn.readLine()).map(_.trim)
      lineOption.foreach {
        case callsPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          callsDb.getCalls(timeFrom, timeTo).foreach(printCall)
        }
        case avgPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          println(callsDb.getAvg(timeFrom, timeTo))
        }
        case totalPattern(from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          println(callsDb.getTotal(timeFrom, timeTo))
        }
        case numberPattern(firstName, lastName) => println(callsDb.getNumber(Name(firstName, lastName)))
        case employeePattern(number) =>  println(callsDb.getEmployee(number.trim))
        case outgoingPattern(firstName, lastName, from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          callsDb.getOutgoing(Name(firstName, lastName), timeFrom, timeTo).foreach(printCall)
        }
        case incomingPattern(firstName, lastName, from, to) => executeFunctionWithTime(from, to) { (timeFrom, timeTo) =>
          callsDb.getIncoming(Name(firstName, lastName), timeFrom, timeTo).foreach(printCall)
        }
        case helpPattern() => println(helpMessage)
        case other => println(s"command '$other' not found")
      }
      shouldContinue = lineOption.isDefined
    }
  }

  private def executeFunctionWithTime(from: String, to: String)
                                     (function: (LocalDateTime, LocalDateTime) => Any): Unit = {
    val (optionFrom, optionTo) = (Option(from), Option(to))
    val tryFrom: Try[LocalDateTime] = optionFrom.map(parseAsLocalDateTime).getOrElse(Success(LocalDateTime.MIN))
    val tryTo: Try[LocalDateTime] = optionTo.map(parseAsLocalDateTime).getOrElse(Success(LocalDateTime.MAX))
    (tryFrom, tryTo) match {
      case (Success(timeFrom), Success(timeTo)) => function(timeFrom, timeTo)
      case _ => println("could not parse time arguments")
    }
  }

  private def parseAsLocalDateTime(time: String): Try[LocalDateTime] = {
    Try(LocalDateTime.parse(time)).orElse {
      Try(LocalDate.parse(time)).map(_.atStartOfDay())
    }.orElse {
      Try(LocalDate.parse(time, DateTimeFormatter.ofPattern("yyyy.MM.dd"))).map(_.atStartOfDay())
    }
  }

  private def printCall(call: Call): Unit = {
    val Call(caller, callee, duration, cost) = call
    println(s"${caller.firstName} | ${caller.lastName} | ${formatNumber(callee)} | $duration | $cost")
  }

  private def formatNumber(number: String): String = {
    number.replaceFirst("(\\d{2})(\\d{4})", "$1 $2 ")
  }
}
