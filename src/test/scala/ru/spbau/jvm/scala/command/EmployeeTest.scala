package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.{Phone, User}

class EmployeeTest extends FlatSpec with Matchers {
  "employee with existing user" should "return user number" in {
    val phones = List(
      new Phone(new Row("1, 1, 123")),
      new Phone(new Row("2, 2, 456"))
    )

    val users = List(
      new User(new Row("1, Will, Smith")),
      new User(new Row("2, Steve, Spielberg"))
    )
    val db = new Billing(List(), phones, users)
    Employee.run(db, Array("employee", "123")) should be("Will Smith")
  }

  "employee with non-existing user" should "return not found" in {
    val phones = List(
      new Phone(new Row("1, 1, 123")),
      new Phone(new Row("2, 2, 456"))
    )

    val users = List(
      new User(new Row("1, Will, Smith")),
      new User(new Row("2, Steve, Spielberg"))
    )
    val db = new Billing(List(), phones, users)
    Employee.run(db, Array("employee", "125")) should be("Cannot find phone 125")
  }
}
