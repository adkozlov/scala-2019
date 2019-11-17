package ru.spbau.jvm.scala

import java.nio.file.{FileSystems, Path}

import slick.jdbc.SQLiteProfile.api._

object Main {
  val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    println("Loading")
    val qr = PhonebookDatabaseInitializer.getPhonebookDatabase(tablesDirPath)
    println("Ready for use!")

    import PhonebookSchema._
    tablesAndFiles.foreach(_._1.statements.foreach(println))

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

//  Instant.parse("2019-12-12")
//  LocalDateTime.ofInstant(Instant.now, ZoneOffset.UTC).compareTo(java.time.LocalDate.of(2019, 11, 17))
}
