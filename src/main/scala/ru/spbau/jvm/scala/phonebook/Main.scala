package ru.spbau.jvm.scala.phonebook

import java.io.IOException
import java.nio.file.Path
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{DateTimeException, LocalDate, LocalDateTime}

import ru.spbau.jvm.scala.phonebook.database.{DatabaseInitializer, Interface}

import scala.io.StdIn.readLine
import scala.util.matching.Regex

object Main {
  private val tablesDirectory = "resources"
  private val phonebookInterface: Interface =
    try {
      DatabaseInitializer.getPhonebookInterface(Path.of(tablesDirectory))
    } catch {
      case e: IOException =>
        println("An error while trying to initialize database. Do you have sqlite3 installed?")
        println(e)
        null
    }

  def main(args: Array[String]): Unit = {
    if (phonebookInterface != null)
      mainLoop()
  }

  def mainLoop(): Unit = {
    def commandRegex(command: String) = s"^[\\s]*$command(.*)".r

    val commands: Seq[(String, String => Unit)] = Seq(
      ("number", dealNumber),
      ("calls", dealCalls),
      ("avg", dealAvg),
      ("total", dealTotal),
      ("user", dealUser),
      ("caller", dealCaller),
      ("internal", dealInternal)
    )

    var exitFlag = false
    while (!exitFlag) {
      val cmd = readLine
      cmd match {
        case "help" => help()
        case "quit" => exitFlag = true
        case "q" => exitFlag = true
        case "schema" => printSchema()
        case command =>
          commands.map { case (commandName, commandDealer) =>
            val reg = commandRegex(commandName)
            () => command match {
              case reg(c) =>
                commandDealer(c)
                true
              case _ =>
                false
            }
          }.foldLeft(false)((matched, b) => matched || b())
      }
    }
  }

  def printSchema(): Unit = {
    import ru.spbau.jvm.scala.phonebook.database.PhonebookSchema._
    tablesAndFiles.foreach(_._1.statements.foreach(println))
  }

  def help(): Unit = {
    val commandsList = Seq(
      ("q", ""),
      ("quit", "exit"),
      ("help", "displays this message"),
      ("avg [from DATE] [to DATE]", "displays average of calls time"),
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
      case numberRegex(number) => phonebookInterface.getNumberUsers(number).foreach {
        case (name, surname) => println(s"$name $surname")
      }
      case _ => println("Number not found")
    }
  }

  def dealCaller(str: String): Unit = {
    val callerRegex = "^[\\s]*([^\\s]+)[\\s]*$".r
    str match {
      case callerRegex(callee) => phonebookInterface.getUsersCalledTo(callee).foreach {
        case (name, surname) => println(s"$name $surname")
      }
      case _ => println("Nobody called this number")
    }
  }

  def dealNumber(str: String): Unit = {
    val nameSurnameRegex = "[\\s]*([^\\s]+)[\\s]*([^\\s]*)[\\s]*".r
    str match {
      case nameSurnameRegex(name, surname) => phonebookInterface.getUserNumbersLeftJoined(name, surname) match {
        case Some(a) => a.foreach(println)
        case None => println(s"employee '$name $surname' not found\n")
      }
      case _ => println("Please specify name [and surname] each in one word")
    }
  }

  def dealTotal(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Total call cost ${niceDatesPeriod(dates)}")
    println(phonebookInterface.getTotal(dates._1, dates._2)
      .map(niceCost)
      .getOrElse("No calls in that period")
    )
  }

  def dealCalls(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Calls ${niceDatesPeriod(dates)}")
    printCalls(phonebookInterface.getCalls(dates._1, dates._2))
  }

  def dealInternal(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Internal calls ${niceDatesPeriod(dates)}")
    printCalls(phonebookInterface.getInternalCalls(dates._1, dates._2))
  }

  def dealAvg(str: String): Unit = {
    val dates = parseDates(str)
    println(s"Average call cost ${niceDatesPeriod(dates)}")
    println(phonebookInterface.getAvg(dates._1, dates._2)
      .map(avg => s"${avg}s")
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
          Option(modifyDate(LocalDate.parse(s).atTime(0, 0)))
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
