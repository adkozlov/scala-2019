package ru.spbau.jvm.scala.database

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.FlatSpec
import ru.spbau.jvm.scala.utils.Utils

class DatabaseTest extends FlatSpec {
  private val testDbPath = "./src/test/resources/databaseTest"
  private val testFiles = Utils.getTxtFilesFromDirectory(testDbPath)
  private val scheme = DbFileParser(testFiles).get

  "DbScheme" should "return the right table" in {
    val empTab = scheme.table("Employees")
    val expectedHeader = DbTableHeader(List(DbAttributeHeader("EmpID", DbTypeInt),
      DbAttributeHeader("FirstName", DbTypeString),
      DbAttributeHeader("LastName", DbTypeString)))
    assert(empTab.dbTableHeader == expectedHeader)
    val expectedContent = new DbTableContent(List(DbTableTuple(List(0, "John", "Doe")),
      DbTableTuple(List(1, "Martin", "King"))))
    assert(empTab.dbTableContent.tuples().toList == expectedContent.tuples().toList)
  }

  "DbScheme" should "fail on a table that does not exist" in {
    assertThrows[DbNoSuchTableException] {
      scheme.table("IAmNotHere")
    }
  }

  "DbTable" should "fail on a column that does not exist" in {
    assertThrows[DbNoSuchColumnException] {
      scheme.table("Employees").column("IAmNotHere")
    }
  }

  "DbTableHeader" should "return correct index by header" in {
    assert(scheme.table("Employees").columnIndex("EmpID") == 0)
    assert(scheme.table("Employees").columnIndex("FirstName") == 1)
    assert(scheme.table("Employees").columnIndex("LastName") == 2)
  }

  "Select" should "return a table with the same element in one of columns" in {
    val dateParser = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val expectedContent = new DbTableContent(List(
      DbTableTuple(List(0, 1, dateParser.parse("2019-11-01 12:00"), "CALL", 10)),
      DbTableTuple(List(0, 2, dateParser.parse("2019-11-01 15:00"), "CALL", 10)),
      DbTableTuple(List(0, 1, dateParser.parse("2019-11-01 16:00"), "CALL", 10)),
      DbTableTuple(List(1, 2, dateParser.parse("2019-11-01 13:00"), "CALL", 10)))).tuples().toList

    assert(scheme.table("PhoneOperations").select("OpType", "CALL").tuples().toList == expectedContent)
  }

  "SelectWhere" should "return a table with predicated elements" in {
    val dateParser = new SimpleDateFormat("yyyy-MM-dd HH:mm")
    val expectedContent = new DbTableContent(List(
      DbTableTuple(List(0, 2, dateParser.parse("2019-11-01 15:00"), "CALL", 10)),
      DbTableTuple(List(0, 1, dateParser.parse("2019-11-01 16:00"), "CALL", 10)),
    )).tuples().toList

    val dateFrom = dateParser.parse("2019-11-01 14:00")
    val res = scheme.table("PhoneOperations").selectWhere("Date", (d: Date) => d.compareTo(dateFrom) > 0).tuples().toList
    assert(res == expectedContent)
  }

  "FoldColumns" should "fold two values of two corresponding columns into one value!" in {
    // Useless exmaple, just for testing.
    val header = DbAttributeHeader("TotalUseless", DbTypeInt)
    val res = scheme.table("EmployeePhoneNumber").foldColumns("EmpID", "PhoneID", header, (a: Int, b: Int) => a + b)
    assert(res.column("TotalUseless") == List(0, 1, 3))
  }

  "Join" should "join two columns" in {
    val empPnTab = scheme.joinOwn("Employees", "EmployeePhoneNumber", "EmpID", "EmpID")

    val expectedHeader = DbTableHeader(List(DbAttributeHeader("EmpID", DbTypeInt),
      DbAttributeHeader("FirstName", DbTypeString),
      DbAttributeHeader("LastName", DbTypeString),
      DbAttributeHeader("PhoneID", DbTypeInt)))
    assert(empPnTab.dbTableHeader == expectedHeader)

    val expectedContent = new DbTableContent(List(
      DbTableTuple(List(0, "John", "Doe", 0)),
      DbTableTuple(List(0, "John", "Doe", 1)),
      DbTableTuple(List(1, "Martin", "King", 2))))
    assert(empPnTab.dbTableContent.tuples().toList == expectedContent.tuples().toList)
  }

}
