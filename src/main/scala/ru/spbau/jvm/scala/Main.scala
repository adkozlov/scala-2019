package ru.spbau.jvm.scala

import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{DateTimeException, LocalDate, LocalDateTime}

import scala.io.StdIn.readLine
import scala.util.matching.Regex

// TODO optimize imports
// TODO remove comments
// TODO unused queries
// TODO TODOs
// TODO make sure every command is implemented
// TODO tests

object Main {
  private val tablesDirectory = "resources"
  private val runner = PhonebookDatabaseInitializer.getPhonebookInterface(Path.of(tablesDirectory))

  def main(args: Array[String]): Unit = {
    mainLoop()
  }

  def mainLoop(): Unit = {
    val numberRegexp = "^[\\s]*number(.*)".r
    val callsRegexp = "^[\\s]*calls(.*)".r
    val avgRegexp = "^[\\s]*avg(.*)".r
    val totalRegexp = "^[\\s]*total(.*)".r
    val userRegexp = "^[\\s]*user(.*)".r
    val callerRegex = "^[\\s]*caller(.*)".r
    val internalRegex = "^[\\s]*internal(.*)".r

    var exitFlag = false
    while (!exitFlag) {
      val cmd = readLine
      cmd match {
        case "help" => help()
        case "quit" => exitFlag = true
        case "q" => exitFlag = true
        case numberRegexp(c) => dealNumber(c)
        case callsRegexp(c) => dealCalls(c)
        case avgRegexp(c) => dealAvg(c)
        case totalRegexp(c) => dealTotal(c)
        case userRegexp(c) => dealUser(c)
        case callerRegex(c) => dealCaller(c)
        case internalRegex(c) => dealInternal(c)
        case "schema" => printSchema()
        case "qwe" => println("qwe")
        case other => invalidResponse(other)
      }
    }
  }

  def printSchema(): Unit = {
    import PhonebookSchema._
    tablesAndFiles.foreach(_._1.statements.foreach(println))
  }

  def help(): Unit = {
    val commandsList = Seq(
      ("q", ""),
      ("quit", "exit"),
      ("help", "displays this message"),
      ("avg [from DATE] [to DATE]", "displays total average of calls costs"),
      ("number NAME [SURNAME]", "displays number, assigned to an employee with provided name and surname"),
      ("calls [from DATE] [to DATE]", "displays calls in specified interval of time. By default dates are from -inf to inf"),
      ("total [from DATE] [to DATE]", "displays total cost of calls in specified interval of time. By default dates are from -inf to current moment"),
      ("user NUMBER", "finds employees who use this number"),
      ("internal [from DATE] [to DATE]", "displays calls in specified period where callee is one of corporation numbers"),
      ("caller NUMBER", "finds employees who have called this number")
    )
    val commandLength = commandsList.map(_._1.length).max

    println("Commands:")
    for ((command, description) <- commandsList) {
      //noinspection ScalaMalformedFormatString
      printf(s"%${-commandLength}s %s\n", command, description)
    }
    println()
    println("Date format is YYYY-MM-DD[Thh:mm[:ss[.millis]]] where T is an actual letter")
  }

  def dealUser(str: String): Unit = {
    val numberRegex = "^[\\s]*([^\\s]+)[\\s]*$".r
    str match {
      case numberRegex(number) => runner.getNumberUsers(number).foreach {
        case (name, surname) => println(s"$name $surname")
      }
      case _ => println("Number not found")
    }
  }

  def dealCaller(str: String): Unit = {
    val callerRegex = "^[\\s]*([^\\s]+)[\\s]*$".r
    str match {
      case callerRegex(callee) => runner.getUsersCalledTo(callee).foreach {
        case (name, surname) => println(s"$name $surname")
      }
      case _ => println("Nobody called this number")
    }
  }

  def dealNumber(str: String): Unit = {
    val nameSurnameRegex = "[\\s]*([^\\s]+)[\\s]*([^\\s]*)[\\s]*".r
    str match {
      case nameSurnameRegex(name, surname) => runner.getUserNumbersLeftJoined(name, surname) match {
        case Some(a) => a.foreach(println)
        case None => println(s"employee '$name $surname' not found\n")
      }
      case _ => println("Please specify name [and surname] each in one word")
    }
  }

  def dealTotal(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Total call cost ${niceDatesPeriod(dates)}")
    println(runner.getTotal(dates._1, dates._2)
      .map(niceCost)
      .getOrElse("No calls in that period")
    )
  }

  def dealCalls(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Calls ${niceDatesPeriod(dates)}")
    printCalls(runner.getCalls(dates._1, dates._2))
  }

  def dealInternal(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Internal calls ${niceDatesPeriod(dates)}")
    printCalls(runner.getInternalCalls(dates._1, dates._2))
  }

  def dealAvg(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Average call cost ${niceDatesPeriod(dates)}")
    println(runner.getAvg(dates._1, dates._2)
      .map(niceCost)
      .getOrElse("No calls in that period")
    )
  }

  def invalidResponse(command: String): Unit = {
    println(s"command '$command' not found")
  }

  def parseDates(command: String): (LocalDateTime, LocalDateTime) = {
    (parseDate(command, "from").getOrElse(defaultFromDate), parseDate(command, "to", d => d.plusDays(1)).getOrElse(defaultToDate))
  }

  def parseDate(str: String, prefix: String, modifyDate: LocalDateTime => LocalDateTime = identity): Option[LocalDateTime] = {
    val prefixR = s"$prefix ".r
    val space = "[\\s]*".r
    val notSpace = "[^\\s]*".r
    afterMatch(str, prefixR) flatMap (afterMatch(_, space)) flatMap (notSpace.findFirstIn(_)) flatMap (s =>
      (
        try {
          Option(LocalDateTime.parse(s))
        } catch {
          case _: DateTimeParseException => Option.empty
        }
      ).orElse(
        try {
          Option(modifyDate(LocalDate.parse(s).atTime(0, 0, 0, 1)))
        } catch {
          case _: DateTimeException => Option.empty
        }
      )
    )
  }

  def afterMatch(s: String, regex: Regex): Option[String] = regex.findFirstMatchIn(s).map(regexpMatch => s.substring(regexpMatch.end))

  private def niceDatesPeriod(dates: (LocalDateTime, LocalDateTime)): String = {
    val formatter = DateTimeFormatter.ofPattern("d MMM uuuu HH:mm:ss")
    s"from ${if (dates._1 == LocalDateTime.MIN) "beginning of time" else formatter.format(dates._1)} to ${formatter.format(dates._2)}"
  }
  private def niceCost(cost: Int): String = {
    val dollars = cost / 100
    val cents = cost % 100
    val centsRepr = (100 + cents).toString.substring(1)
    s"$dollars.$centsRepr$$"
  }

  private def printCalls(calls: Seq[((Int, String, String, Int), (Int, String, Int, Int, String))]): Unit = {
    println("FirstName | LastName | Callee | Duration (s) | Cost ($) | Time")
    calls.foreach {
      case ((_, name, surname, _), (_, callee, time, cost, datetime)) =>
        println(s"$name | $surname | $callee | $time | ${niceCost(cost)} | $datetime")
    }
  }

  private def defaultFromDate = LocalDateTime.MIN
  private def defaultToDate = LocalDateTime.now()
}