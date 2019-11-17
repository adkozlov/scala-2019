package ru.spbau.jvm.scala

//import slick.model.Table
import java.io.{File, OutputStreamWriter}
import java.nio.file.FileSystems
import java.sql.DriverManager

import ru.spbau.jvm.scala.Main1.{connect, initDatabase}
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

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

import Tables._

object Main {
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
       |.import ${FileSystems.getDefault.getPath("resources").resolve("User.txt")} User
       |.import ${FileSystems.getDefault.getPath("resources").resolve("Number.txt")} Number
    """.stripMargin

  def initDatabase(databaseFilePath: String): Unit = {
    val process = Runtime.getRuntime.exec(s"sqlite3 $databaseFilePath")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(sqliteInit)
    writer.close()
    process.waitFor()
  }

  def connect(databaseFilePath: String) = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    DriverManager.getConnection(url)
  }

  def main(args: Array[String]): Unit = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()
    println(s"temp file name:${tempFile.getPath}")


    val conn = connect(tempFile.getPath)
    initDatabase(tempFile.getPath)
    val url = s"jdbc:sqlite:file:${tempFile.getPath}"

    val db = Database.forURL(url)


//    val query = users.map(p => (p.id,p.name,p.number_id))
//    db.run(query.result).foreach(s => s.foreach(p => println(p)))

//    val s = Await.result(db.run(query.result), Duration.Inf)
//    s.foreach(p => println(p))
val query = users.map(p => (p.id,p.name,p.number_id))

    val tRes = Await.result(db.run(query.result), Duration.Inf)
    println(tRes)

//    val sql =
//    //#sqlQueryProjection*
//      sql"select * from PERSON".as[User]
//    //#sqlQueryProjection*
//    val slick =
//      users.result
//
//    val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)

//    users.map(p => println(p.id, p.name, p.surname, p.number_id)).result
//     //    conn.close()
//
  }
}

