package ru.spbau.jvm.scala.db

import org.scalatest.{FlatSpec, Matchers}

class DBSpec extends FlatSpec with Matchers {
  val db = new BillingDB
  db.loadTables("resources")

  "Users table" should "have same size as at file" in {
    db.users.size should be(11)
  }

  "Phones table" should "have same size as at file" in {
    db.phones.size should be(12)
  }

  "Calls table" should "have same size as at file" in {
    db.calls.size should be(19)
  }
}
