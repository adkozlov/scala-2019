package ru.spbau.jvm.scala.billing

import java.io.File
import java.util.Date

import ru.spbau.jvm.scala.cli.{DateRange, PersonName}
import ru.spbau.jvm.scala.database._

import scala.util.{Failure, Success}

case class SmsStat(sentCount: Int, gotCount: Int)

class BillingSystemPersonNotFoundException(person: PersonName) extends Exception {
  override def getMessage: String = s"Employee $person not found"
}

class BillingSystemEmptyDateRangeException(dateRange: DateRange) extends Exception {
  override def getMessage: String = s"Nothing has been found in date range $dateRange"
}

class BillingNoContactNotFoundException(caller: PersonName, callee: PersonName) extends Exception {
  override def getMessage: String = s"Employee $caller has not contacts with $callee"
}

/*
Queries:
* avg [duration | cost] [from <date> [<date-to>]] -- add average call cost
* calls [from <date-from> [<date-to>]] -- all calls
* total [from <date-from> [<date-to>]] -- total cost
* number <first-name> <last-name> -- number of an employee <first-name> <last-name>
* messages [from <date-from> [<date-to>]] -- all messages
* contacts <name> to <name> [from <date-from [to <date-to>]] -- count of contacts between two people
* help -- print help
*/
class BillingSystem(private val databaseFiles: List[File]) {
  private val Employees = "Employees"
  private val EmployeePhoneNumbers = "EmployeePhoneNumbers"
  private val PhoneNumbersPool = "PhoneNumbersPool"
  private val PhoneOperations = "PhoneOperations"
  private val Tariffs = "Tariffs"

  private val scheme = DbFileParser(databaseFiles) match {
    case Failure(ex) => throw ex
    case Success(scheme) => scheme
  }

  def avgDuration(dateRange: DateRange): Double = {
    val durations = selectDate(dateRange, scheme.table(PhoneOperations).select("OpType", "CALL")).column("Duration")
    durations.asInstanceOf[List[Int]].sum / durations.length
  }

  private def selectDate(dateRange: DateRange, table: DbTable): DbTable = {
    try {
      table.selectWhere("Date", datePred(dateRange))
    } catch {
      case _: DbSelectException => throw new BillingSystemEmptyDateRangeException(dateRange)
    }
  }

  private def datePred(dateRange: DateRange): Date => Boolean = {
    dateRange match {
      case DateRange(None, None) => (_: Date) => true
      case DateRange(Some(dateFrom), None) => (d: Date) => d.compareTo(dateFrom) > 0
      case DateRange(None, Some(dateTo)) => (d: Date) => d.compareTo(dateTo) < 0
      case DateRange(Some(dateFrom), Some(dateTo)) => (d: Date) => d.compareTo(dateFrom) > 0 && d.compareTo(dateTo) < 0
    }
  }

  def avgCost(dateRange: DateRange): Double = {
    val opCallTab = scheme.table(PhoneOperations).select("OpType", "CALL")

    val totalCostI =
      selectDate(dateRange, scheme.join(opCallTab, scheme.table(Tariffs), "PhoneID", "PhoneID")
        .select("TariffType", "CALL"))
        .selectByNames(List("Duration", "Cost")) map { row =>
        row.tuple(0).asInstanceOf[Int] * row.tuple(1).asInstanceOf[Float]
      }
    val totalCost = totalCostI.toList
    totalCost.sum / totalCost.length
  }

  def calls(dateRange: DateRange): List[List[Any]] = {
    val t1 = scheme.table(PhoneOperations).select("OpType", "CALL")
    val t2 = scheme.join(t1, scheme.table(Tariffs), "PhoneID", "PhoneID")
      .select("TariffType", "CALL")
      .foldColumns("Duration", "Cost", DbAttributeHeader("TotalCost", DbTypeFloat), (a: Int, b: Float) => a * b)

    selectDate(dateRange, scheme.join(
      scheme.join(
        scheme.join(t2, scheme.table(EmployeePhoneNumbers), "PhoneID", "PhoneID"),
        scheme.table(Employees), "EmpID", "EmpID"),
      scheme.table(PhoneNumbersPool), "DestPhoneID", "PhoneID"))
      .selectByNames(List("FirstName", "LastName", "PhoneNumber", "Duration", "TotalCost"))
      .map(_.tuple).toList
  }

  def messages(person: PersonName, dateRange: DateRange): SmsStat = {
    val empId = selectPerson(person).column("EmpID").head
    val tabSent =
      selectDate(dateRange, scheme.joinOwn(EmployeePhoneNumbers, PhoneOperations, "PhoneID", "PhoneID")
        .select("OpType", "SMS"))
    val sentSms = tabSent.select("EmpID", empId).column("EmpID").length

    val tabGot =
      selectDate(dateRange, scheme.joinOwn(EmployeePhoneNumbers, PhoneOperations, "PhoneID", "DestPhoneID")
        .select("OpType", "SMS"))
    val gotSms = tabGot.select("EmpID", empId).column("EmpID").length

    SmsStat(sentSms, gotSms)
  }

  def total(dateRange: DateRange): Float = {
    selectDate(dateRange, scheme.joinOwn(PhoneOperations, Tariffs, "PhoneID", "PhoneID"))
      .selectWhereBi("OpType", "TariffType", (a: String, b: String) => a == b)
      .foldColumns("Duration", "Cost", DbAttributeHeader("TotalCost", DbTypeFloat), (a: Int, b: Float) => a * b)
      .column("TotalCost").asInstanceOf[List[Float]].sum
  }

  def number(person: PersonName): List[String] = {
    val empPhone = scheme.join(selectPerson(person), scheme.table(EmployeePhoneNumbers), "EmpID", "EmpID")
    scheme.join(empPhone, scheme.table(PhoneNumbersPool), "PhoneID", "PhoneID")
      .column("PhoneNumber").asInstanceOf[List[String]]
  }

  private def selectPerson(person: PersonName): DbTable = {
    try {
      scheme.table(Employees).select("LastName", person.lastName).select("FirstName", person.firstName)
    } catch {
      case _: DbSelectException => throw new BillingSystemPersonNotFoundException(person)
    }
  }

  def contact(caller: PersonName, callee: PersonName, dateRange: DateRange): Int = {
    val empIdCaller = selectPerson(caller).column("EmpID").head
    val empIdCallee = selectPerson(callee).column("EmpID").head
    val tab = selectDate(dateRange, scheme.joinOwn(PhoneOperations, EmployeePhoneNumbers, "PhoneID", "PhoneID"))
      .select("EmpID", empIdCaller)
      .removeColumn("EmpID")

    try {
      selectDate(dateRange, scheme.join(tab, scheme.table(EmployeePhoneNumbers), "DestPhoneID", "PhoneID"))
        .select("EmpID", empIdCallee).tuples().length
    } catch {
      case _: DbSelectException => throw new BillingNoContactNotFoundException(caller, callee)
    }
  }
}
