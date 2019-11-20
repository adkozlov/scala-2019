package ru.spbau.jvm.scala

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable
import scala.io.Source

object BillingDatabase {
  type PhoneNumber = String
  private val DateFormat = new SimpleDateFormat("dd.MM.yyyy")

  private val employeesTablePath = "resources/employees.txt"
  private val phonePoolTablePath = "resources/phone_pool.txt"
  private val callsTablePath = "resources/calls.txt"

  private val employees = mutable.HashSet[Employee]()
  private val employeePhones = mutable.HashMap[Employee, PhoneNumber]()
  private var calls =  List[Call]()

  private def loadEmployeesTable() = {
    employees.clear()
    val employeeFromId = mutable.HashMap[Int, Employee]()
    val employeesTable = Source.fromFile(employeesTablePath)
    for (line <- employeesTable.getLines()) {
      val columns = line.split(',')
      val employee = Employee(columns(1), columns(2))
      employees.add(employee)
      employeeFromId.put(columns(0).toInt, employee)
    }
    employeesTable.close()
    employeeFromId
  }

  private def loadPhonePoolTable(employeeFromId: mutable.HashMap[Int, Employee]) = {
    employeePhones.clear()
    val employeeFromPhoneId = mutable.HashMap[Int, Employee]()
    val phonePoolTable = Source.fromFile(phonePoolTablePath)
    for (line <- phonePoolTable.getLines()) {
      val columns = line.split(',')
      if (columns(2) != "NULL") {
        val employee = employeeFromId(columns(2).toInt)
        employeePhones.put(employee, columns(1))
        employeeFromPhoneId.put(columns(0).toInt, employee)
      }
    }
    phonePoolTable.close()
    employeeFromPhoneId
  }

  private def loadCallsTable(employeeFromPhoneId: mutable.HashMap[Int, Employee]): Unit = {
    val callsTable = Source.fromFile(callsTablePath)
    calls = callsTable.getLines().map(_.split(','))
      .map(columns => Call(
        employeeFromPhoneId(columns(1).toInt),
        columns(2),
        DateFormat.parse(columns(3)),
        columns(4).toInt,
        columns(5).toDouble))
      .toList
    callsTable.close()
  }

  def loadDatabase(): Unit = {
    val employeeFromId = loadEmployeesTable()
    val employeeFromPhoneId = loadPhonePoolTable(employeeFromId)
    loadCallsTable(employeeFromPhoneId)
  }

  def getCallsBetweenDates(fromDate: Date, toDate: Date): List[Call] =
    calls.filter(call => (call.date.after(fromDate) || call.date.equals(fromDate)) &&
      (call.date.before(toDate) || call.date.equals(toDate)))

  def getAverageCallDuration: Int = calls.map(_.duration).sum / calls.length

  def getTotalCallCostBetweenDates(fromDate: Date, toDate: Date): Double =
    getCallsBetweenDates(fromDate, toDate).map(_.cost).sum

  def getEmployeePhone(firstName: String, lastName: String): PhoneNumber = {
    val employee = Employee(firstName, lastName)
    if (!employees.contains(employee))
      throw new IllegalArgumentException(s"Employee $firstName $lastName not found")
    if (!employeePhones.contains(employee))
      throw new IllegalArgumentException(s"Employee $firstName $lastName does not have a corporate phone number")
    employeePhones(employee)
  }

  def getTotalCostForEachEmployee: List[(Employee, Double)] =
    calls.groupMapReduce(_.caller)(_.cost)((cost1, cost2) => cost1 + cost2).toList

  def getCallsByEmployee(firstName: String, lastName: String): List[Call] = {
    val employee = Employee(firstName, lastName)
    calls.filter(call => call.caller.equals(employee))
  }

  def getEmployeeWithHighestTotalCost: Option[(Employee, Double)] =
    getTotalCostForEachEmployee.maxByOption(_._2)(Ordering.Double.TotalOrdering)
}