package ru.spbau.jvm.scala.phonebook.database

import java.io.{File, OutputStreamWriter}
import java.nio.file.Path

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import slick.jdbc.SQLiteProfile.api._

object DatabaseInitializer {

  def getPhonebookInterface(tablesDirectory: Path): Interface = {
    val tempDBFile = createDatabaseFile()
    val db = connectDatabase(tempDBFile)

    createDatabaseSchema(db)
    fillDatabase(tablesDirectory, tempDBFile)

    new Interface(db)
  }

  private def createDatabaseFile(): String = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()
    tempFile.getPath
  }

  private def connectDatabase(databaseFilePath: String): Database = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    Database.forURL(url)
  }

  private def createDatabaseSchema(db: Database): Unit = {
    import PhonebookSchema.tablesAndFiles

    Await.result(db.run(DBIO.seq(
      tablesAndFiles.map(_._1):_*
    )), Duration.Inf)
  }

  private def fillDatabase(tablesDirectory: Path, tempDBFile: String): Unit = {
    val process = Runtime.getRuntime.exec(s"sqlite3 $tempDBFile")
    val writer = new OutputStreamWriter(process.getOutputStream)

    val importLines = PhonebookSchema.tablesAndFiles.map(_._2).map(tableName => s".import ${tablesDirectory.resolve(s"$tableName.txt")} $tableName")

    writer.write(".mode csv\n")
    writer.write(importLines.mkString("\n"))
    writer.close()
    process.waitFor()

    // copying stream to a string with scanner
    val s = new java.util.Scanner(process.getInputStream).useDelimiter("\\A")
    val errors = if (s.hasNext()) s.next() else ""

    if (!errors.isEmpty) {
      println("Errors while loading tables:")
      println(errors)
      println("Proceeding anyway")
    }
  }
}
