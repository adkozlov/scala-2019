package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.scalatest._

class DatabaseSpec extends FlatSpec with Matchers {
  Database.loadDatabase()

  "Average call length" should "be average" in {
    Database.avg() should be (7)
  }

  "Total cost of service" should "be sum of the selected range" in {
    Database.costs(DateTime.parse("2019-11-15T13:26:41.659+03:00"), DateTime.parse("2019-11-15T13:36:42.659+03:00")) should be (6.5f)
    Database.costs(DateTime.parse("2019-11-15T13:26:41.659+03:00")) should be (7.7f)
    Database.costs() should be (7.8f)
  }

  "Users" should "have a correct numbers" in {
    Database.number("Natela", "Evangelista") should be ("+7 951 555 39 05")
    Database.number("Unknown", "User") should be (null)
  }

  "Unused numbers" should "be present" in {
    Database.unusedNumbers() should be (List("+7 900 123 43 21", "+7 900 765 43 21"))
  }

  "Unique callers" should "be present" in {
    Database.uniqueCallees() should be (Set("+7 922 327 78 74"))
  }

  "All users" should "call this number" in {
    Database.whoCallsOnThisNumber("+7 922 327 78 74") should be (Set(User("Jamesina","Lykke"),
      User("Ishani","Sargsyan"), User("Riko","Ingersleben"), User("Kassandra","Olesen"), User("Hebe","Pasternack"),
      User("Febe","Simek"), User("Almira","O'Byrne"), User("Agaue","Lithgow"), User("Marina","Causer"), User("Natela","Evangelista")))
  }

  "Call's list" should "contain names instead of phones" in {
    Database.callList() should be (List(
      NamedCall(User("Agaue","Lithgow"),"+7 922 327 78 74",4,0.4f),
      NamedCall(User("Natela","Evangelista"),"+7 922 327 78 74",6,0.6f),
      NamedCall(User("Almira","O'Byrne"),"+7 922 327 78 74",10,1.0f),
      NamedCall(User("Marina","Causer"),"+7 922 327 78 74",9,0.9f),
      NamedCall(User("Ishani","Sargsyan"),"+7 922 327 78 74",11,1.1f),
      NamedCall(User("Jamesina","Lykke"),"+7 922 327 78 74",7,0.7f),
      NamedCall(User("Febe","Simek"),"+7 922 327 78 74",12,1.2f),
      NamedCall(User("Riko","Ingersleben"),"+7 922 327 78 74",5,0.5f),
      NamedCall(User("Kassandra","Olesen"),"+7 922 327 78 74",2,0.2f),
      NamedCall(User("Kassandra","Olesen"),"+7 922 327 78 74",1,0.1f),
      NamedCall(User("Kassandra","Olesen"),"+7 922 327 78 74",3,0.3f),
      NamedCall(User("Hebe","Pasternack"),"+7 922 327 78 74",8,0.8f)))
  }
}
