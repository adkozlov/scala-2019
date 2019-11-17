package ru.spbau.jvm.scala

import java.nio.file.{FileSystems, Path}
import java.time.{DateTimeException, LocalDate, LocalDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import scala.io.StdIn.readLine
import scala.util.matching.Regex

// TODO optimize imports
// TODO remove comments
// TODO foreign keys?
// TODO unused queries
// TODO TODOs
// TODO make sure every command is implemented
// TODO tests

object Main {
  private val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
//    println(parseDate("  dsda kek 2020-10-10T12:16:23 dlkajslka", "kek"))
    mainLoop()
  }

  def mainLoop(): Unit = {
    val numberRegexp = "^\\s*number ".r
    val callsRegexp = "^\\s*calls".r
    val avgRegexp = "^^\\s*avg\\s*$".r

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
        case "schema" => printSchema()
        case _ => invalidResponse(_)
      }
    }
  }

  def dealNumber(c: String): Unit = {

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
      ("avg", "displays total average of calls costs"),
      ("number NAME SURNAME", "displays number, assigned to employee with provided name"),
      ("calls [from DATE] [to DATE]", "displays calls in specified interval of time. By default dates are from -inf to inf"),
      ("total [from DATE] [to DATE]", "displays total cost of calls in specified interval of time. By default dates are from -inf to inf")
    ) // TODO
    val commandLength = commandsList.map(_._1.length).max

    println("Commands:")
    for ((command, description) <- commandsList) {
      //noinspection ScalaMalformedFormatString
      printf(s"%${-commandLength}s %s\n", command, description)
    }
  }

  def dealCalls(str: String): Unit = {

  }

  def dealAvg(str: String): Unit = {

  }

  def invalidResponse(command: String): Unit = {
    println(s"command '$command' not found")
  }

  private def parseDates(command: String): (Option[LocalDateTime], Option[LocalDateTime]) = {
    (parseDate(command, "from"), parseDate(command, "to", d => d.plusDays(1)))
  }

  def afterMatch(s: String, regex: Regex): Option[String] = regex.findFirstMatchIn(s).map(regexpMatch => s.substring(regexpMatch.end))

  def parseDate(str: String, prefix: String, modifyDate: LocalDateTime => LocalDateTime = identity): Option[LocalDateTime] = {
    val prefixR = s"$prefix ".r
    val space = "[\\s]*".r
    val notSpace = "[^\\s]*".r
    afterMatch(str, prefixR) flatMap (afterMatch(_, space)) flatMap (notSpace.findFirstIn(_)) flatMap (s =>
      (
        try {
          Option(LocalDateTime.parse(s))
        } catch {
          case _: DateTimeParseException => Option.empty[LocalDateTime]
        }
        ).orElse(
        try {
          Option(modifyDate(LocalDate.parse(s).atTime(0, 0)))
        } catch {
          case _: DateTimeException => Option.empty[LocalDateTime]
        }
      )
    )
  }
}