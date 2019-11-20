package ru.spbau.jvm.scala

import java.time.LocalDateTime

import org.scalatest.FlatSpec

class InterpreterTests extends FlatSpec {
  "CallsTimeInterval " should "return calls in interval" in {
    val from = LocalDateTime.parse("2019-11-16T12:00:00")
    val to = LocalDateTime.parse("2019-11-16T23:00:00")
    val calls = CallTable().getCallsTimeInterval(from, to)
    assert(calls.size === 3)
    val sumSeconds = calls.map(call => call.duration.getSeconds).sum
    assert(sumSeconds === 195)
  }

  "CallTable" should "load entire file" in {
    val calls = CallTable().all
    val callsAsStrings = calls.map(call => call.toString)
    assert(calls.size === 10)
    assert(callsAsStrings.contains("79007654321 | 79001234321 | 2019-11-16T19:48:01+03:00 | 108 | 1.08"))
  }

  "User table" should "be fully loaded" in {
    val users = UserTable().all
    val Gurevich = users.filter(user => user.firstName == "Alexandr" && user.lastName == "Gurevich")
    assert(Gurevich.size === 1)
    val Doe = users.filter(user => user.firstName == "John" && user.lastName == "Doe")
    assert(Doe.size === 1)
  }
}
