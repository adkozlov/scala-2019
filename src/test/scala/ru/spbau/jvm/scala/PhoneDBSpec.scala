package ru.spbau.jvm.scala

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest._

class PhoneDBSpec extends FlatSpec with Matchers {
  def getResultsOfCalls(inputCalls: String): String = {
    val result: ByteArrayOutputStream = new ByteArrayOutputStream(1000)
    Console.withIn(new ByteArrayInputStream(inputCalls.getBytes)) {
      Console.withOut(result) {
        PhoneDB.main(null)
      }
    }
    Console.flush()

    result.toString()
  }

  "calls from FIRSTDATE to LASTDATE" should "be some calls" in {
    val input: String =
      """|calls from 02.09.2019 to 03.09.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """|FirstName | LastName | Callee | Duration (s) | Cost ($)
         |Andrey | Popov | Phone(+7 066 666 66 66) | 2 | 300
         |Dima | Ivanov | Phone(+7 033 333 33 33) | 5 | 200
         |""".stripMargin)
  }

  "calls from LATE_DATE to EARLY_DATE" should "be empty" in {
    val input: String =
      """|calls from 10.09.2019 to 01.09.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """|FirstName | LastName | Callee | Duration (s) | Cost ($)
         |""".stripMargin)
  }

  "calls from DATE_TIME" should "print all calls after the first date" in {
    val input: String =
      """|calls from 10.09.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """|FirstName | LastName | Callee | Duration (s) | Cost ($)
         |Enot | Enotov | Phone(+7 022 222 22 22) | 1 | 100
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 800
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 13 | 100
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
         |""".stripMargin)
  }

  "calls to DATE_TIME" should "print all calls before the date" in {
    val input: String =
      """|calls to 01.09.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """|FirstName | LastName | Callee | Duration (s) | Cost ($)
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 5 | 100
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
         |""".stripMargin)
  }

  "calls" should "print all calls" in {
    val input: String =
      """|calls
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """|FirstName | LastName | Callee | Duration (s) | Cost ($)
         |Enot | Enotov | Phone(+7 022 222 22 22) | 1 | 100
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 800
         |Andrey | Popov | Phone(+7 066 666 66 66) | 2 | 300
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 13 | 100
         |Dima | Ivanov | Phone(+7 033 333 33 33) | 8 | 500
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
         |Dima | Ivanov | Phone(+7 033 333 33 33) | 5 | 200
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 5 | 100
         |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
         |""".stripMargin)
  }

  "avg" should "average cost of calls" in {
    val input: String =
      """|avg
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be ("18s\n")
  }

  "avg from DATE_TIME to DATE_TIME" should "average cost calls in given range" in {
    val input: String =
      """|avg from 01.09.2019 to 01.09.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be ("24s\n")
  }

  "total from DATETIME to DATETIME" should "print total cost in range" in {
    val input: String =
      """|total from 02.09.2019 to 03.10.2019
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be ("2000\n")
  }

  "total" should "print total cost" in {
    val input: String =
      """|total
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be ("2300\n")
  }

  "help" should "print help" in {
    val input: String =
      """|help
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """calls from DATETIME to DATETIME : список всех звонков за заданный промежуток времени
        |avg from DATETIME to DATETIME: средняя длительность звонка
        |total from DATETIME to DATETIME: суммарная стоимость услуг связи за заданный промежуток времени
        |number VARCHAR VARCHAR: номер телефона заданного сотрудника
        |help: вызов справки
        |quit: выход из программы
        |
        |
        |name VARCHAR: пишет имя сотрудника по номеру телефона
        |all users: пишет всех сотрудников и их номера телефонов
        |calls of VARCHAR: пишет все вызовы указанного сотрудника
        |""".stripMargin)
  }

  "number of existed employee" should "print his number" in {
    val input: String =
      """|number Sasha Gladkov
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be(
      """+7 111 111 11 11
        |""".stripMargin)
  }

    "number of unknown employee" should "print error" in {
      val input: String =
        """|number Unknown Mister
           |quit
           |""".stripMargin
      getResultsOfCalls(input) should be (
        """employee 'Unknown Mister' not found
          |""".stripMargin)
  }

  "user of existed number" should "print user" in {
    val input: String =
      """|user +7 111 111 11 11
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be(
      """Sasha Gladkov
        |""".stripMargin)
  }

  "user of unknown number" should "print error" in {
    val input: String =
      """|user +7 123 456 78 90
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """phone '+7 123 456 78 90' not found
        |""".stripMargin)
  }

  "all users" should "print all users" in {
    val input: String =
      """|all users
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """Sasha Gladkov +7 111 111 11 11
        |Dima Ivanov +7 333 333 33 33
        |Enot Enotov +7 222 222 22 22
        |Anton Zaytsev +7 777 777 77 77
        |Andrey Popov +7 666 666 66 66
        |Timofey Bryksin +7 444 444 44 44
        |Leonid Semenov +7 999 999 99 99
        |""".stripMargin)
  }

  "calls of" should "print all calls of user" in {
    val input: String =
      """|calls of Sasha Gladkov
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """FirstName | LastName | Callee | Duration (s) | Cost ($)
        |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 800
        |Sasha | Gladkov | Phone(+7 011 111 11 11) | 13 | 100
        |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
        |Sasha | Gladkov | Phone(+7 011 111 11 11) | 5 | 100
        |Sasha | Gladkov | Phone(+7 011 111 11 11) | 42 | 100
        |""".stripMargin)
  }

  "calls of unknown user" should "print error" in {
    val input: String =
      """|calls of Unknown Mister
         |quit
         |""".stripMargin
    getResultsOfCalls(input) should be (
      """employee 'Unknown Mister' not found
        |""".stripMargin)
  }
}
