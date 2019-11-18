package ru.spbau.jvm.scala

import org.joda.time.DateTime

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Database(private val callsFileName: String,
               private val employeesFileName: String) {

  private final val SEPARATOR = ","
  private val calls = new ArrayBuffer[Call]
  private val employeeToPhones = new mutable.HashMap[Employee, Phone]
  private val phoneToEmployee = new mutable.HashMap[Phone, Employee]

  load()

  private def loadCalls(): Unit = {
    val callsFile = Source.fromFile(callsFileName)

    for (line <- callsFile.getLines) {
      line.split(SEPARATOR) match {
        case Array(caller, callee, date, duration, cost) =>
          val call = Call(caller, callee, DateTime.parse(date), duration.toInt, cost.toFloat)
          calls.addOne(call)
        case _ => throw new IllegalStateException
      }
    }

    callsFile.close
  }

  private def loadEmployees(): Unit = {
    val employeesFile = Source.fromFile(employeesFileName)

    for (line <- employeesFile.getLines) {
      line.split(SEPARATOR) match {
        case Array(firstName, lastName, phone) =>
          val employee = Employee(firstName, lastName)
          if (phoneToEmployee.contains(phone) || employeeToPhones.contains(employee)) {
            throw new IllegalStateException
          }
          phoneToEmployee.addOne((phone, employee))
          employeeToPhones.addOne(employee, phone)
        case _ => throw new IllegalStateException
      }
    }

    employeesFile.close
  }

  private def load(): Unit = {
    loadCalls()
    loadEmployees()
  }

  def calls(from: DateTime, to: DateTime): List[EmployeeCall] = {
    calls
      .filter(call => from.compareTo(call.date) <= 0 && to.compareTo(call.date) >= 0)
      .map(call =>
        EmployeeCall(phoneToEmployee.getOrElse(call.caller, throw new IllegalStateException),
          call.callee, call.duration, call.cost))
      .toList
  }

  def averageCallDuration(): Double =
    if (calls.isEmpty) 0 else calls.map(call => call.duration).sum / calls.size

  def totalCost(from: DateTime = new DateTime(Long.MinValue),
                to: DateTime = new DateTime(Long.MaxValue)): Double = calls
    .filter(call => from.compareTo(call.date) <= 0 && to.compareTo(call.date) >= 0)
    .map(call => call.cost.toDouble)
    .sum

  def getPhone(employee: Employee): Option[Phone] = employeeToPhones.get(employee)

  def callsFromEmployee(employee: Employee): Option[List[Call]] = {
    employeeToPhones.get(employee) match {
      case Some(caller) => Some(calls.filter(call => call.caller.equals(caller)).toList)
      case None => None
    }
  }

  def groupCallsByEmployee(from: DateTime, to: DateTime): List[EmployeeTotal] = {
    val filteredCalls = calls(from, to)
    filteredCalls
      .map(call => EmployeeTotal(call.employee, call.duration, call.cost))
      .groupBy(call => call.employee)
      .map(p => p._2)
      .map(employeeCalls => employeeCalls.reduce((callTotal, call) => callTotal + call))
      .toList
  }

  def getEmployees: List[(Employee, Phone)] = employeeToPhones.toList
}