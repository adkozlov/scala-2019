package ru.spbau.jvm.scala

import java.io.{File, OutputStreamWriter}
import java.nio.file.Path

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object PhonebookDatabaseInitializer {
  def getPhonebookDatabase(tablesDirectory: Path): PhonebookQueryRunner = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()

    val db = connectDatabase(tempFile.getPath)

    Await.result(db.run(DBIO.seq(
      PhonebookSchema.numbers.schema.create,
      PhonebookSchema.users.schema.create,
      PhonebookSchema.calls.schema.create
    )), Duration.Inf)

    val qr = new PhonebookQueryRunner(db)

    val process = Runtime.getRuntime.exec(s"sqlite3 ${tempFile.getPath}")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(s"""
         |.mode csv
         |.import ${tablesDirectory.resolve("User.txt")} User
         |.import ${tablesDirectory.resolve("Number.txt")} Number
         |.import ${tablesDirectory.resolve("Call.txt")} Call
    """.stripMargin)
    writer.close()
    process.waitFor()

    qr
  }

  private def connectDatabase(databaseFilePath: String): Database = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    Database.forURL(url)
  }
}

class PhonebookQueryRunner(database: Database) {
  def run[R](query: Query[_, R, Seq]) = database.run(query.result)
  def getQueryResult[R](query: Query[_, R, Seq]) = Await.result(run(query), Duration.Inf)
}
