package ru.spbau.jvm.scala

import org.scalatest._

class DatabaseSpec extends FlatSpec with Matchers {

  "Databases reading" should "not throw exceptions" in {
    Database.readDatabases()
  }

  "Get calls" should "get calls in date interval" in {
    Database.getCalls(Database.dateFormat.parse("2019-11-20"), Database.dateFormat.parse("2019-11-21")) shouldBe
      Array[Call](Call.apply("88005553531", "57573236523", User.apply("88005553531", "Roberto", "Otrebor"), null, Database.dateTimeFormat.parse("2019-11-20 17:41:08"), 24, 4.56),
                   Call.apply("88005553537", "89035353413", User.apply("88005553537", "Anton", "Notna"), null, Database.dateTimeFormat.parse("2019-11-21 17:42:08"), 434, 3.24))
  }

  "Average call duration" should "be average" in {
    Database.getAverageDuration shouldBe 213.0
  }

  "Total cost" should "be total cost in date interval" in {
    Database.getTotalCost(Database.dateFormat.parse("2019-11-20"), Database.dateFormat.parse("2019-11-21")) shouldBe 7.8
  }

  "Employee number" should "be correct" in {
    Database.getEmployeeNumber("Franz", "Znarf") shouldBe "88005553534"
    Database.getEmployeeNumber("Kek", "Lol") shouldBe null
  }

  "Employee calls" should "get all employee calls" in {
    Database.getEmployeeCalls("Alberto", "Otrebla") shouldBe
      Array[Call](Call.apply("88005553532", "88005553535", User.apply("88005553532", "Alberto", "Otrebla"), User.apply("88005553535", "Rolf", "Flor"), Database.dateTimeFormat.parse("2019-11-22 17:44:08"), 76, 0.26))
  }

  "Inner calls" should "get all calls from employee to employee" in {
    Database.getInnerCalls shouldBe Array[Call](
      Call.apply("88005553533", "88005553537", User.apply("88005553533", "Marvin", "Nivram"), User.apply("88005553537", "Anton", "Notna"), Database.dateTimeFormat.parse("2019-11-19 17:44:08"), 100, 10.12),
      Call.apply("88005553532", "88005553535", User.apply("88005553532", "Alberto", "Otrebla"), User.apply("88005553535", "Rolf", "Flor"), Database.dateTimeFormat.parse("2019-11-22 17:44:08"), 76, 0.26)
    )
  }

  "Spender" should "be an employee which spent the most amount of money" in {
    Database.getSpender shouldBe (User.apply("88005553534", "Franz", "Znarf"), 23.45)
  }
}
