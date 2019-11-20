package ru.spbau.jvm.scala
import java.time._

import scala.io.Source
import java.time.format.DateTimeFormatter

class Table(val employees : Array[Employee], val calls: Array[Call]) {
    private val minDate = calls.map( call => call.date ).min
    private val maxDate = calls.map( call => call.date ).max

    private def callsInTimePeriod(startDate: LocalDate, endDate: LocalDate): Array[Call] = {
        val start = if (startDate == null) minDate else startDate
        val end = if (endDate == null) maxDate else endDate
        calls.filter( call => call.date.compareTo(end) <= 0 && call.date.compareTo(start) >= 0 )
    }
    
    def calls(startDate: LocalDate, endDate: LocalDate): Array[(Employee, Call)] =
        employees.flatMap(employee => callsInTimePeriod(startDate, endDate).filter(_.number == employee.number).map(call => (employee, call)))
    
    def avg(startDate: LocalDate, endDate: LocalDate): Int = {
        val callsInPeriod = callsInTimePeriod(startDate, endDate)
        callsInPeriod.map( _.duration ).sum / callsInPeriod.length
    }

    def numberHolder(number: String): Employee = employees.find(_.number == number).orNull

    def callsByEmployee(name: String, secondName: String): Array[Call] = {
        val employee = employees.find(employee => employee.name == name && employee.secondName == secondName).orNull
        if (employee == null) null
        else calls.filter( _.number == employee.number )
    }

    def count(startDate: LocalDate, endDate: LocalDate): Int = callsInTimePeriod(startDate, endDate).length

    def number(name: String, secondName: String): String = {
        val employee = employees.find(employee => employee.name == name && employee.secondName == secondName).orNull
        if (employee == null) null
        else employee.number
    }

    def total(start: LocalDate, end: LocalDate): Double = callsInTimePeriod(start, end).map( _.cost ).sum
}

class Employee(val name: String, val secondName: String, val number: String) {}

object Employee {
    def parseFromString(tableLine: String): Employee = {
        val parsedList = tableLine.split("[|;]")
        if (parsedList.length != 3) {
            throw new IllegalArgumentException(parsedList.length + " elements given, but only 3 expected")
        }
        new Employee(parsedList(0), parsedList(1), parsedList(2))
    }
}

class Call(val number: String, val date: LocalDate, val duration: Int, val cost: Double) {}

object Call {
    def parseFromString(tableLine: String): Call = {
        val parsedList :Array[String] = tableLine.split("[|;]").map(_.trim)
        if (parsedList.length != 4) {
            throw new IllegalArgumentException(parsedList.length + " elements given, but only 3 expected")
        }
        val dateFormat = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        new Call(parsedList(0), LocalDate.parse(parsedList(1), dateFormat), parsedList(2).toInt, parsedList(3).toDouble)
    }
}

object InputReader {
    def readEmployees(filename: String): Array[Employee] = 
        Source.fromFile(filename).getLines.drop(1).map( line => Employee.parseFromString(line) ).toArray

    def readCalls(filename: String): Array[Call] =
        Source.fromFile(filename).getLines.drop(1).map( line => Call.parseFromString(line) ).toArray
}


