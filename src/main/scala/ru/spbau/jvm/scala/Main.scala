package ru.spbau.jvm.scala

import slick.jdbc.SQLiteProfile.api._
import java.nio.file.{FileSystems, Path}

object Main {
  val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    println("Loading")
    val qr = PhonebookDatabaseInitializer.getPhonebookDatabase(tablesDirPath)
    println("Ready for use!")

    import PhonebookSchema._
    tablesAndFiles.foreach(_._1.statements.foreach(println))

    import PhonebookQueries._

    for (_ <- 1 to 1)
    {
      val q = qr.database.run(calls.map(p => p.user_id).result)
//      val tRes = qr.getQueryResult(selectAllUsers)
      val tRes = qr.getQueryResult(calls.map(p => (p.user_id, p.callee, p.datetime)))
      println(tRes)
    }
  }

}
