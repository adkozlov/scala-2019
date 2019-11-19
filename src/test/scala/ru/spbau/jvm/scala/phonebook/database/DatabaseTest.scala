package ru.spbau.jvm.scala.phonebook.database

import java.io.File
import java.nio.file.{Files, Path}
import java.time.{LocalDate, LocalDateTime}

import org.scalatest.{BeforeAndAfterEach, FunSuite}

class DatabaseTest extends FunSuite with BeforeAndAfterEach {

  private var tempDir: Path = _
  private var callFile: File = _
  private var numberFile: File = _
  private var userFile: File = _
  private var interface: Interface = _

  override def beforeEach(): Unit = {
    if (callFile != null) callFile.delete()
    if (numberFile != null) numberFile.delete()
    if (userFile != null) userFile.delete()
    if (tempDir != null) tempDir.toFile.delete()

    tempDir = Files.createTempDirectory("phonebookTest")
    callFile = tempDir.resolve("Call.txt").toFile
    numberFile = tempDir.resolve("Number.txt").toFile
    userFile = tempDir.resolve("User.txt").toFile

    callFile.createNewFile()
    numberFile.createNewFile()
    userFile.createNewFile()

    populateUsers()
    populateNumbers()
    populateCalls()

    interface = DatabaseInitializer.getPhonebookInterface(tempDir)
  }

  test("interface initializes") {
    // more of a test test
  }

  test("correct number of all calls") {
    assert(8 == interface.getCalls(LocalDateTime.MIN, LocalDateTime.of(2020, 12, 12, 12, 12)).length)
  }

  test("correct number of calls in a period of time") {
    val dateFrom = LocalDateTime.of(2018, 11, 15, 0, 0, 0)
    val dateTo = dateFrom.plusDays(1)
    assert(5 == interface.getCalls(dateFrom, dateTo).length)
  }

  private val dateFrom2019 = LocalDateTime.of(2019, 1, 1, 0, 0)
  private val dateTo2019 = dateFrom2019.plusYears(1)

  test("computes total right") {
    val totalCost: Double = 99 + 1 + 9
    assert(totalCost == interface.getTotal(dateFrom2019, dateTo2019).get)
  }

  test("computes avg right") {
    val avgCost: Double = (99 + 1 + 9) / 3
    assert(avgCost == interface.getAvg(dateFrom2019, dateTo2019).get)
  }

  test("avg is none if it should be") {
    val dateFrom = LocalDateTime.of(3019, 1, 1, 0, 0)
    val dateTo = dateFrom.plusYears(1)
    assert(interface.getAvg(dateFrom, dateTo).isEmpty)
  }

  test("gets right user by number") {
    assert(Seq(("Rat", "Geravker")) == interface.getNumberUsers("kjk"))
  }

  test("gets right caller") {
    assert(Seq(("Uno", "Unov")) == interface.getUsersCalledTo("101"))
  }

  test("gets unique caller") {
    assert(Seq(("Ivanov", "Ivan")) == interface.getUsersCalledTo("404"))
  }

  test("gets multiple callers") {
    val returnedCallers = interface.getUsersCalledTo("kjk")
    assert(Seq(("Ivanov", "Ivan"), ("Uno", "Unov")) == returnedCallers || Seq(("Uno", "Unov"), ("Ivanov", "Ivan")) == returnedCallers)
  }

  test("gets all internal calls") {
    val internals = interface.getInternalCalls(LocalDateTime.MIN, LocalDateTime.of(3030,12,12,12,12))
    assert(internals.map(_._2._2 == "kjk").forall(identity))
  }

  test("gets internal calls in period") {
    val dateFrom = LocalDateTime.of(2018, 11, 15, 18, 20, 27)
    val internals = interface.getInternalCalls(dateFrom, dateFrom.plusSeconds(5))
    assert(internals.length == 1)
  }

  private def populateUsers(): Unit = {
    Files.writeString(userFile.toPath,
      """
        |0,Ivanov,Ivan,0
        |1,Uno,Unov,2
        |2,Rat,Geravker,1
        |3,Another,One,3
      """.stripMargin)
  }

  private def populateNumbers(): Unit = {
    Files.writeString(numberFile.toPath,
      """
        |0,88005553535
        |1,kjk
        |2,asdsd
        |3,3
      """.stripMargin)
  }

  private def populateCalls(): Unit = {
    Files.writeString(callFile.toPath,
      """
        |1,8(800) 555-35-35,228,99,2019-02-03 18:20:28.661
        |0,404,123,9,2019-11-17 18:20:28.661
        |0,404,123,1,2019-11-15 18:20:28.661
        |0,404,123,100,2018-11-15 18:20:28.661
        |1,3344,123,100,2018-11-15 18:20:28
        |1,1010,123,100, ERROR, should not affect anything
        |1,101,123,100,2018-11-15 18:20:28
        |1,kjk,123,100,2018-11-15 18:20:26
        |0,kjk,123,100,2018-11-15 18:20:28
      """.stripMargin)
  }

}

object Runner extends App {
  org.scalatest.run(new DatabaseTest())
}