package ru.spbau.jvm.scala

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}

import scala.collection.mutable
import scala.io.Source

case class Number(name: String, phoneNumber: String)

case class Call(callerNumber: Number,
                calleePhoneNumber: String,
                date: DateTime,
                duration: Int,
                cost: Double)

class Database {
  val numbersList = new mutable.ArrayBuffer[Number]()
  val callsList = new mutable.ArrayBuffer[Call]()
  val nameToNumber = new mutable.HashMap[String, String]
  val numberToName = new mutable.HashMap[String, String]
  val numberToCalls = new mutable.HashMap[Number, mutable.HashSet[Call]]

  def loadDatabase(usersFilename: String, callsFilename: String): Unit = {
    val usersFile = Source.fromFile(usersFilename)
    val callsFile = Source.fromFile(callsFilename)
    usersFile
      .getLines()
      .foreach(
        row =>
          row.split(",") match {
            case Array(name, phone) => numbersList.addOne(Number(name, phone))
            case _ =>
              throw new IllegalStateException(
                "Bad format in resources/users.csv"
              )
        }
      )
    numbersList.foreach(number => {
      val nameToNumberElement = nameToNumber.get(number.name)
      if (nameToNumberElement.isDefined) {
        throw new IllegalStateException("Man has 2 or more numbers")
      } else {
        nameToNumber.put(number.name, number.phoneNumber)
      }

      val numberToNameElement = numberToName.get(number.name)
      if (numberToNameElement.isDefined) {
        throw new IllegalStateException(
          "The same phone number belongs to 2 or more people"
        )
      } else {
        numberToName.put(number.phoneNumber, number.name)
      }
    })
    callsFile
      .getLines()
      .foreach(
        row =>
          row.split(",") match {
            case Array(callerPhone, calleePhone, date, duration, cost) =>
              val call = Call(
                Number(
                  numberToName.getOrElse(callerPhone, "Undefined Caller Name"),
                  callerPhone
                ),
                calleePhone,
                DateTime.parse(date, Database.formatter),
                duration.toInt,
                cost.toDouble
              )
              callsList.addOne(call)
              val numberToCallsElement = numberToCalls.get(call.callerNumber)
              if (numberToCallsElement.isDefined) {
                numberToCallsElement.get.add(call)
              } else {
                numberToCalls
                  .put(call.callerNumber, mutable.HashSet[Call](call))
              }
            case _ =>
              throw new IllegalStateException(
                "Bad format in resources/users.csv"
              )
        }
      )
    usersFile.close()
    callsFile.close()
  }
}

object Database {
  val formatter: DateTimeFormatter = DateTimeFormat.forPattern("dd.MM.yyyy")
}
