package ru.spbau.jvm.scala

import java.io.{File, OutputStreamWriter}
import java.nio.file.Path

import slick.jdbc.SQLiteProfile.api._


object PhonebookDatabaseInitializer {

  def initDatabase(tablesDirectory: Path): Database = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()

    val process = Runtime.getRuntime.exec(s"sqlite3 ${tempFile.getPath}")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(s"""
         |.mode csv
         |create table User (
         |    id INTEGER not null primary key autoincrement,
         |    name varchar(128) not null,
         |    surname varchar(128) not null,
         |    number_id INTEGER not null
         |  );
         |create table Number (
         |    id INTEGER not null primary key,
         |    number varchar(128) not null
         |  );
         |.import ${tablesDirectory.resolve("User.txt")} User
         |.import ${tablesDirectory.resolve("Number.txt")} Number
    """.stripMargin)
    writer.close()
    process.waitFor()

    getDatabase(tempFile.getPath)
  }

  private def getDatabase(databaseFilePath: String): Database = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    Database.forURL(url)
  }
}
