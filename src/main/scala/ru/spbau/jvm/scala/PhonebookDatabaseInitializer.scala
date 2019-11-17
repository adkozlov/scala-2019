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

    val process = Runtime.getRuntime.exec(s"sqlite3 ${tempFile.getPath}")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(s"""
         |.mode csv
         |create table Number (
         |    id INTEGER NOT NULL PRIMARY KEY,
         |    number TEXT NOT NULL
         |  );
         |create table User (
         |    id INTEGER NOT NULL PRIMARY KEY,
         |    name TEXT NOT NULL,
         |    surname TEXT NOT NULL,
         |    number_id INTEGER NOT NULL REFERENCES Number,
         |    UNIQUE(name, surname)
         |  );
         | create table Call (
         |    user_id INTEGER NOT NULL REFERENCES User,
         |    callee TEXT NOT NULL,
         |    duration_s INT NOT NULL,
         |    cost_c INT NOT NULL
         |  );
         |.import ${tablesDirectory.resolve("User.txt")} User
         |.import ${tablesDirectory.resolve("Number.txt")} Number
         |.import ${tablesDirectory.resolve("Call.txt")} Call
    """.stripMargin)
    writer.close()
    process.waitFor()

    new PhonebookQueryRunner(connectDatabase(tempFile.getPath))
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