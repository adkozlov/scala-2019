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
import Tables._

object Main {
  val tablesDirPath: Path = FileSystems.getDefault.getPath("resources")

  def main(args: Array[String]): Unit = {
    val db = PhonebookDatabaseInitializer.initDatabase(tablesDirPath)

    val query = users.map(p => (p.id,p.name,p.number_id))

    val tRes = Await.result(db.run(query.result), Duration.Inf)
    println(tRes)
  }
}
