package ru.spbau.jvm.scala

import java.nio.file.{FileSystems, Path}

import slick.jdbc.SQLiteProfile.api._

import scala.io.StdIn.readLine

// TODO optimize imports
// TODO remove comments
// TODO foreign keys?
// TODO unused queries
// TODO TODOs

object Main {
  private val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    val qr = PhonebookDatabaseInitializer.getPhonebookDatabase(tablesDirPath)

//    import PhonebookSchema._ // TODO remove
//    tablesAndFiles.foreach(_._1.statements.foreach(println)) // TODO remove

    mainLoop()

    import java.time.LocalDate

    import PhonebookQueries._

    val localDate: LocalDate = LocalDate.parse("2019-11-10")

    for (_ <- 1 to 1)
    {
//      val dt = LocalDateTime.of(2019, 10, 15, 0, 1)

      val tRes = qr.run(userNumberQuery2("Kolyan", "The Great").result)
      println(tRes)
    }

    userNumberJoin.result.statements.foreach(println)
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
        case _ => invalidResponse(_)
      }
    }
  }

  def dealNumber(c: String): Unit = {

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
}
