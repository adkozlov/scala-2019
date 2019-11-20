package ru.spbau.jvm.scala.database

import java.text.SimpleDateFormat

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.utils.Utils

class DbFileParserTest extends FlatSpec {
  private val testDbPath = "./src/test/resources/databaseTest"
  private val testFiles = Utils.getTxtFilesFromDirectory(testDbPath)
  private val scheme = DbFileParser(testFiles)

  "Parsed scheme" should "be correct" in {
    assert(scheme.isSuccess)
  }

  "`Employees` table" should "be correct" in {
    val empTab = scheme.get.table("Employees")
    val expectedHeader = DbTableHeader(List(DbAttributeHeader("EmpID", DbTypeInt),
      DbAttributeHeader("FirstName", DbTypeString),
      DbAttributeHeader("LastName", DbTypeString)))
    assert(empTab.dbTableHeader == expectedHeader)
    val expectedContent = new DbTableContent(List(DbTableTuple(List(0, "John", "Doe")),
      DbTableTuple(List(1, "Martin", "King"))))
    assert(empTab.dbTableContent.tuples().toList == expectedContent.tuples().toList)
  }

  "`PhoneOperations` table" should "be correct" in {
    val empTab = scheme.get.table("PhoneOperations")
    val expectedHeader = DbTableHeader(List(DbAttributeHeader("PhoneID", DbTypeInt),
      DbAttributeHeader("DestPhoneID", DbTypeInt),
      DbAttributeHeader("Date", DbTypeDate),
      DbAttributeHeader("OpType", DbTypeString),
      DbAttributeHeader("Duration", DbTypeInt)))
    assert(empTab.dbTableHeader == expectedHeader)

    val dateParser = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val expectedContent = new DbTableContent(List(
      DbTableTuple(List(0, 1, dateParser.parse("2019-11-01 12:00"), "CALL", 10)),
      DbTableTuple(List(0, 2, dateParser.parse("2019-11-01 15:00"), "CALL", 10)),
      DbTableTuple(List(0, 1, dateParser.parse("2019-11-01 16:00"), "CALL", 10)),
      DbTableTuple(List(1, 2, dateParser.parse("2019-11-01 13:00"), "CALL", 10)),
      DbTableTuple(List(2, 0, dateParser.parse("2019-11-01 10:00"), "SMS", 1))))
    assert(empTab.dbTableContent.tuples().toList == expectedContent.tuples().toList)
  }

  /* Other tables contains the sames types, so it enough to test only `Employees` and `PhoneOperations`. */
}