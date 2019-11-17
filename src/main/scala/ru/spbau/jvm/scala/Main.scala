package ru.spbau.jvm.scala

import java.nio.file.{FileSystems, Path}

import ru.spbau.jvm.scala.PhonebookSchema._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Main {
  val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    println("Loading")
    val db = PhonebookDatabaseInitializer.getPhonebookDatabase(tablesDirPath)
    println("Ready for use!")


    for (_ <- 1 to 10)
    {
      val pb = new PhonebookQueries(db)
      val tRes = pb.getQueryResult(selectAllUsers)
      println(tRes)
    }
  }
}
