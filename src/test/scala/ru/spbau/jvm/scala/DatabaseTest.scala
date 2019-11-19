package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}

class DatabaseTest extends FlatSpec with Matchers {

  private val database = new Database("src/test/resources/PhoneUser.txt",
    "src/test/resources/Operation.txt",
    "src/test/resources/Price.txt")
  private val emptyDatabase = new Database("src/test/resources/empty.txt",
    "src/test/resources/empty.txt", "src/test/resources/empty.txt")

  "lastCall" should "return last day when selected user called someone" in {
    database.lastCall("Alice", "ALice").get should be (Date(16, 11, 2019))
    database.lastCall("Bob", "Bob").get should be (Date(19, 11, 2019))
  }

  "lastCall" should "return None if there is no such user or this user didn't call" in {
    assert(database.lastCall("NoSuch", "User").isEmpty)
    assert(database.lastCall("User", "WithNoOperations").isEmpty)
  }

  "total" should "return sum in a period" in {
    database.total(DateRange(Date(10, 10, 2019), Date(12, 10, 2019))) should be (221.2)
    database.total(DateRange(Date(10, 10, 10), Date(20, 12, 20))) should be (0)
  }

  "average" should "show average call duration" in {
    database.average() should be (37.9)
  }

  "average on file with no calls" should "be zero" in {
    emptyDatabase.average() should be (0)
  }

  "number" should "return list of all numbers with given owner" in {
    database.number("NoSuch", "User") should be (List.empty[String])
    database.number("Alice", "ALice") should be (List("+8 888 888 88 88"))
    database.number("Bob", "Bob") should be (List("+9 999 999 99 99"))
    database.number("Ivan", "Ivanov") should be (List("28197", "8917837"))
  }

  "NULL NULL user" should "be interpreted as no user" in {
    database.number("NULL", "NULL") should be (List.empty[String])
  }

  "freeNumbers" should "return list of all free numbers" in {
    emptyDatabase.freeNumbers() should be (List.empty[String])
    database.freeNumbers() should be (List("8903210", "89037219"))
  }

  "sortUsers" should "return users in sorted order with their outlays" in {
    emptyDatabase.sortUsers() should be (List.empty[(String, String, Double)])
    database.sortUsers() should be (List(("Aba", "Caba", 4455.800000000001),
                                         ("Bob", "Bob", 478.0),
                                         ("Alice", "ALice", 376.0)))
  }

  "calls" should "return list of all calls from given period" in {
    database.calls(DateRange(Date(15, 11, 2019), Date(16, 11, 2019))) should
      be (List(CallResult(Person("Aba", "Caba"), "8 800 555 35 35", 42, 84.0),
        CallResult(Person("Aba", "Caba"), "8 800 555 35 35", 3, 6.0),
        CallResult(Person("Alice", "ALice"), "8 800 555 35 35", 42, 84.0),
        CallResult(Person("Alice", "ALice"), "8 800 555 35 35", 3, 6.0),
        CallResult(Person("Alice", "ALice"), "8 800 555 35 35", 1, 2.0)))
    emptyDatabase.calls(DateRange(minDate, maxDate)) should be (List.empty[CallResult])
  }
}
