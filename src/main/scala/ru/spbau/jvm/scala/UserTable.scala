package ru.spbau.jvm.scala

import scala.io.Source

class UserTable(users: List[User]) {
  def all: List[User] = users

  def usersByPhoneNumber(phoneNumber: String): List[User] = {
    users.filter(user => user.phoneNumber == phoneNumber).toList
  }
}

object UserTable {
  def apply(): UserTable = {
    val userSource = Source.fromFile("resources/users.csv")
    val users = userSource.getLines().drop(1)
      .map(_.split(","))
      .map(userFromRecord)
    new UserTable(users.toList)
  }

  private def userFromRecord(record: Array[String]): User = {
    val firstName = record(0)
    val secondName = record(1)
    val phoneNumber = record(2).replaceAll("[^\\d]", "")
    new User(firstName, secondName, phoneNumber)
  }
}