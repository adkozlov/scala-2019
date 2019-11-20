package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.scalatest._

class DatabaseTest extends FlatSpec with BeforeAndAfterAll with Matchers {

  private var database: Database = _

  override def beforeAll() {
    database = new Database()
    database.load()
  }

  "getCallsAverageDuration" should "return correct average duration of all calls" in {
    assert(database.getCallsAverageDuration === 1500.0)
  }

  "getMaxDuration" should "return correct max duration of all calls" in {
    assert(database.getMaxDuration.get === 1800.0)
  }

  "getMinDuration" should "return correct min duration of all calls" in {
    assert(database.getMinDuration.get === 1200.0)
  }

  "getPhoneFromUser" should "return empty if there is no phone for user" in {
    assert(database.getPhoneFromUser("ZZZ", "ZZ").isEmpty)
  }

  "getPhoneFromUser" should "return correct phone if there is phone for user" in {
    assert(database.getPhoneFromUser("AAA", "AA").get === "+79999999990")
  }

  "getUserFromPhone" should "return correct name and surname if there is user for phone" in {
    assert(database.getUserFromPhone("+79999999990").get === ("AAA", "AA"))
  }

  "getUserFromPhone" should "return empty if there is no user for phone" in {
    assert(database.getUserFromPhone("+70000000000").isEmpty)
  }

  "getCostInPeriod" should "be correct" in {
    assert(database.getCostInPeriod(DateTime.parse("2011-10-13"), DateTime.parse("2011-10-20")) === 9000.0)
    assert(database.getCostInPeriod(DateTime.parse("2011-10-13")) === 15600.0)
    assert(database.getCostInPeriod() === 21000.0)
    assert(database.getCostInPeriod(DateTime.parse("2020-10-13")) === 0.0)
  }

  "getCallsInPeriod with from and to specified" should "return list of users' calls in given period" in {
    database.getCallsInPeriod(DateTime.parse("2011-10-13"), DateTime.parse("2011-10-20")) should
      be (List(("EEE", "EE", "+79999999994", 1600, 14.0),
      ("FFF", "FF", "+79999999995", 1700, 15.0),
      ("GGG", "GG", "+79999999996", 1800, 16.0),
      ("AAA", "AA", "+79999999990", 1200, 10.0),
      ("BBB", "BB", "+79999999991", 1300, 11.0),
      ("CCC", "CC", "+79999999992", 1400, 12.0)))
  }

  "getCallsInPeriod with only from specified" should "return list of users' calls from given data" in {
    database.getCallsInPeriod(DateTime.parse("2011-10-13")) should
      be (List(("EEE", "EE", "+79999999994", 1600, 14.0),
        ("FFF", "FF", "+79999999995", 1700, 15.0),
        ("GGG", "GG", "+79999999996", 1800, 16.0),
        ("AAA", "AA", "+79999999990", 1200, 10.0),
        ("BBB", "BB", "+79999999991", 1300, 11.0),
        ("CCC", "CC", "+79999999992", 1400, 12.0),
        ("DDD", "DD", "+79999999993", 1500, 13.0),
        ("EEE", "EE", "+79999999994", 1600, 14.0),
        ("FFF", "FF", "+79999999995", 1700, 15.0),
        ("GGG", "GG", "+79999999996", 1800, 16.0)))
  }

  "getCallsInPeriod with only from specified" should "return list of all users' calls" in {
    database.getCallsInPeriod() should
      be (List(("AAA", "AA", "+79999999990", 1200, 10.0),
        ("BBB", "BB", "+79999999991", 1300, 11.0),
        ("CCC", "CC", "+79999999992", 1400, 12.0),
        ("DDD", "DD", "+79999999993", 1500, 13.0),
        ("EEE", "EE", "+79999999994", 1600, 14.0),
        ("FFF", "FF", "+79999999995", 1700, 15.0),
        ("GGG", "GG", "+79999999996", 1800, 16.0),
        ("AAA", "AA", "+79999999990", 1200, 10.0),
        ("BBB", "BB", "+79999999991", 1300, 11.0),
        ("CCC", "CC", "+79999999992", 1400, 12.0),
        ("DDD", "DD", "+79999999993", 1500, 13.0),
        ("EEE", "EE", "+79999999994", 1600, 14.0),
        ("FFF", "FF", "+79999999995", 1700, 15.0),
        ("GGG", "GG", "+79999999996", 1800, 16.0)))
  }
}
