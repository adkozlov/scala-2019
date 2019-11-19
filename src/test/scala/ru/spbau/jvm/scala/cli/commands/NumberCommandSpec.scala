package ru.spbau.jvm.scala.cli.commands

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.db.BillingDB
import ru.spbau.jvm.scala.db.rows.{PhoneRow, UserRow}

class NumberCommandSpec extends FlatSpec with Matchers {
  val db = new BillingDB
  val numberCommand = new NumberCommand

  "Number command name" should "be \"number\"" in {
    numberCommand.name should be("number")
  }

  "number command info" should
    "be \"number VARCHAR VARCHAR -- номер телефона заданного сотрудника\"" in {
    numberCommand.info should be("number VARCHAR VARCHAR -- номер телефона заданного сотрудника")
  }

  "number command with existing user" should "return number of user" in {
    db.phones += PhoneRow("1", "1", "123")
    db.phones += PhoneRow("2", "2", "321")
    db.users += UserRow("1", "FirstName1", "LastName1")
    db.users += UserRow("2", "FirstName2", "LastName2")
    numberCommand.execute(db, Array("number", "FirstName1", "LastName1")) should be(
      ("numbers",
        Iterable[String](
          "123"
        ))
    )
  }

  "number command with non-existing user" should "return error" in {
    db.phones += PhoneRow("1", "1", "123")
    db.phones += PhoneRow("2", "2", "321")
    db.users += UserRow("1", "FirstName1", "LastName1")
    db.users += UserRow("2", "FirstName2", "LastName2")
    numberCommand.execute(db, Array("number", "FirstName3", "LastName3")) should be(
      ("employee 'FirstName3 LastName3' not found", Iterable[String]())
    )
  }
}
