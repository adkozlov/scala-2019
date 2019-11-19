package ru.spbau.jvm.scala.db.tables

import ru.spbau.jvm.scala.db.rows.UserRow

class UsersTable extends Table[UserRow] {
  override def parseRow(line: String): UserRow = {
    line.split(",") match {
      case Array(id: String, firstName: String, lastName: String) =>
        UserRow(id, firstName, lastName)
      case _ => throw new IllegalArgumentException("Invalid user line")
    }
  }
}
