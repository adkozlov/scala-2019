package ru.spbau.jvm.scala

import slick.jdbc.SQLiteProfile.api._


object PhonebookSchema {
  class User(tag: Tag) extends Table[(Int, String, String, Int)](tag, "User") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def surname = column[String]("surname")
    def number_id = column[Int]("number_id")

    override def * = (id, name, surname, number_id)
    def number = foreignKey("number_fk", number_id, numbers)(_.id)
  }

  class PhoneNumber(tag: Tag) extends Table[(Int, String)](tag, "Number") {
    def id = column[Int]("id")
    def number = column[String]("number")
    override def * = (id, number)
  }

  class Call(tag: Tag) extends Table[(Int, String, Int, Int, String)](tag, "Call") {
    def user_id = column[Int]("user_id")
    def callee = column[String]("callee")
    def time = column[Int]("time_s")
    def cost = column[Int]("cost")
    def datetime = column[String]("datetime")
    override def * = (user_id, callee, time, cost, datetime)

    def user = foreignKey("user_fk", user_id, users)(_.id)
  }

  lazy val users = TableQuery[User]
  lazy val numbers = TableQuery[PhoneNumber]
  lazy val calls = TableQuery[Call]

  final val tablesAndFiles = Seq(
    (numbers.schema.create, "Number"),
    (users.schema.create, "User"),
    (calls.schema.create, "Call")
  )
}