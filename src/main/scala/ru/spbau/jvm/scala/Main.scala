package ru.spbau.jvm.scala

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import ru.spbau.jvm.scala.CallsDB.Call

import scala.io.StdIn
import scala.util.{Success, Try}

object Main {
  private val fromToLiteral = "(?:\\s+from ([0-9.\\-]+))?(?:\\s+to ([0-9.\\-]+))?"
  private val callsPattern = s"calls$fromToLiteral".r
  private val avgPattern = "avg(?:\\s+from ([0-9.\\-]+))?(?:\\s+to ([0-9.\\-]+))?".r

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
    println(s"${caller.firstName} | ${caller.lastName} | $callee | $duration | $cost")
  }
}
