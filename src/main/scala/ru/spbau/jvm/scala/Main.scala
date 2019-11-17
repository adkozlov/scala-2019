package ru.spbau.jvm.scala

import java.io.{File, OutputStreamWriter}
import java.nio.file.{FileSystems, Path}

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Tables {
  class User(tag: Tag) extends Table[(Int, String, String, Int)](tag, "User") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def surname = column[String]("surname")
    def number_id = column[Int]("number_id")

    override def * = (id, name, surname, number_id)
  }

  class PhoneNumber(tag: Tag) extends Table[(Int, String)](tag, "Number") {
    def id = column[Int]("id")
    def number = column[String]("number")
    override def * = (id, number)
  }

  lazy val users = TableQuery[User]
  lazy val numbers = TableQuery[PhoneNumber]
}

import ru.spbau.jvm.scala.Tables._

object Main {
  val tablesFolderPath: Path = FileSystems.getDefault.getPath("resources")
  val sqliteInit: String =
    s"""
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
       |.import ${tablesFolderPath.resolve("User.txt")} User
       |.import ${tablesFolderPath.resolve("Number.txt")} Number
    """.stripMargin

  def initDatabase(): String = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()

    val process = Runtime.getRuntime.exec(s"sqlite3 ${tempFile.getPath}")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(sqliteInit)
    writer.close()
    process.waitFor()

    tempFile.getPath
  }

  def getDatabase(databaseFilePath: String): Database = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    Database.forURL(url)
  }

  def main(args: Array[String]): Unit = {
    val db = getDatabase(initDatabase())

    val query = users.map(p => (p.id,p.name,p.number_id))

    val tRes = Await.result(db.run(query.result), Duration.Inf)
    println(tRes)
  }
}
