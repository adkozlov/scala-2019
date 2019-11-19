package ru.spbau.jvm.scala.command

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing
import ru.spbau.jvm.scala.storage.db.Row
import ru.spbau.jvm.scala.storage.db.orm.{Call, Phone, User}

class NumberTest extends FlatSpec with Matchers {
  "number with existing user" should "return user number" in {
    val phones = List(
      new Phone(new Row("1, 1, 123")),
      new Phone(new Row("2, 2, 456"))
    )

    val users = List(
      new User(new Row("1, Will, Smith")),
      new User(new Row("2, Steve, Spielberg"))
    )
    val db = new Billing(List(), phones, users)
    Number.run(db, Array("number", "Will", "Smith")) should be("123")
  }

  "number with non-existing user" should "return not found" in {
    val phones = List(
      new Phone(new Row("1, 1, 123")),
      new Phone(new Row("2, 2, 456"))
    )

    val users = List(
      new User(new Row("1, Will, Smith")),
      new User(new Row("2, Steve, Spielberg"))
    )
    val db = new Billing(List(), phones, users)
    Number.run(db, Array("number", "Will", "Spielberg")) should be("Employee Will Spielberg not found")
  }
}
