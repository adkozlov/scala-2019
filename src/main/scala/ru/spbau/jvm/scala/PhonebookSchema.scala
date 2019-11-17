package ru.spbau.jvm.scala

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PhonebookSchema {
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

object PhonebookQueries {
  import PhonebookSchema._

  def selectAllUsers = users.map(p => (p.id, p.name, p.number_id))
}

class PhonebookQueryRunner(database: Database) {
  def getQueryResult[R](query: Query[_, R, Seq]) = Await.result(database.run(query.result), Duration.Inf)
}