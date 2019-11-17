package ru.spbau.jvm.scala

import java.nio.file.{FileSystems, Path}

object Main {
  val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    println("Loading")
    val qr = PhonebookDatabaseInitializer.getPhonebookDatabase(tablesDirPath)
    println("Ready for use!")

    import PhonebookQueries._

    for (_ <- 1 to 10)
    {
      val tRes = qr.getQueryResult(selectAllUsers)
      println(tRes)
    }
  }

}
