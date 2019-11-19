package ru.spbau.jvm.scala

import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.storage.Billing

class BillingLoadTest extends FlatSpec with Matchers {
  val billing: Billing = Billing.load(new File("resources/calls.txt"), new File("resources/phones.txt"), new File("resources/users.txt"))

  "Users size in memory" should "be equal to size in file" in {
    billing.users.size should be(11)
  }

  "Phones size in memory" should "be equal to size in file" in {
    billing.phones.size should be(11)
  }

  "Calls size in memory" should "be equal to size in file" in {
    billing.calls.size should be(10)
  }
}
