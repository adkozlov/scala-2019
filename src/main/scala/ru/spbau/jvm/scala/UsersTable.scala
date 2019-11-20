package ru.spbau.jvm.scala

case class User(var name: String,
                var surname: String,
                var phone: String) extends TableEntity

class UsersTable extends Table[User] {

  override def parseEntity(line: String): User = {
    line.split(SEPARATOR) match {
      case Array(name: String, surname: String, phone: String) =>
        User(name, surname, phone)
      case _ =>
        throw new DatabaseException("Invalid line format. Line should match (name,surname,phone).")
    }
  }
}
