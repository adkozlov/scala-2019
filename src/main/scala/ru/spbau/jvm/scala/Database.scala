package ru.spbau.jvm.scala

import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class DatabaseException(message: String) extends Exception(message)

/** Types of operations */
object Operation extends Enumeration {
  type Operation = Value
  val MESSAGE, INTERNET, CALL = Value
}
import Operation._

case class Person(firstName: String, lastName: String)

/** One row in PhoneUser table */
case class PhoneUserRow(person: Option[Person], phone: String)

/**
 * One row in Operation table.
 *
 * Item is property of operation for which money is debited.
 *    For calls it is call duration in seconds,
 *    for messages -- number of messages,
 *    for internet -- downloaded bytes
 */
case class OperationRow(phone: String, operationType: Operation,
                        date: Date, callee: String, items: Int)

/** One row in Price table */
case class PriceRow(operationType: Operation, price: Double)

case class CallResult(person: Person, callee: String, duration: Int, cost: Double)

class Database(private val phoneUserFilename: String = "resources/PhoneUser.txt",
               private val operationFilename: String = "resources/Operation.txt",
               private val priceFilename: String = "reources/Price.txt") {
  private val phoneUserTable = loadPhoneUser()
  private val operationTable = loadOperation()
  private val priceTable = loadPrice()
  private val phoneUserJoinOperation = joinPhoneUserOperation()
  private val operationJoinPrice = joinOperationPrice()
  private val allJoin = joinAll()

  def average(): Double = {
    var totalDuration = 0
    var callsNumber = 0
    for (operation <- operationTable) {
      if (operation.operationType == CALL) {
        totalDuration += operation.items
        callsNumber += 1
      }
    }
    if (callsNumber != 0) {
      totalDuration.toDouble / callsNumber
    } else {
      0
    }
  }

  /** Returns all numbers that persons with given name and surname have */
  def number(firstName: String, lastName: String): List[String] =
    phoneUserTable.filter(row => row.person.isDefined).filter(row => {
      val person = row.person.get
      person.firstName == firstName && person.lastName == lastName
    }).map(row => row.phone).toList

  def calls(dateRange: DateRange): List[CallResult] = {
    val buffer = ListBuffer.empty[CallResult]
    for ((userId, operationId, priceId) <- allJoin) {
      val operation = operationTable(operationId)
      if (operation.operationType == CALL && dateRange.contains(operation.date)) {
        val price = priceTable(priceId)
        val user = phoneUserTable(userId)
        buffer.addOne(CallResult(user.person.get, operation.callee,
          operation.items, operation.items * price.price))
      }
    }
    buffer.toList
  }

  /**
   * Returns list of all users who made at least one operation
   *    sorted by decrease of their outlay
   */
  def sortUsers(): List[(String, String, Double)] = {
    val userToSum = new mutable.HashMap[Int, Double]
    for ((userId, operationId, priceId) <- allJoin) {
      val operationPrice = operationTable(operationId).items * priceTable(priceId).price
      val oldSum = userToSum.getOrElseUpdate(userId, 0)
      userToSum.put(userId, oldSum + operationPrice)
    }
    val list = userToSum.toList
    val sorted = list.sorted(Ordering.by((_: (Int, Double))._2).reverse)
    val result = ListBuffer.empty[(String, String, Double)]
    for ((userId, outlay) <- sorted) {
      val person = phoneUserTable(userId).person.get
      result.addOne((person.firstName, person.lastName, outlay))
    }
    result.toList
  }

  def freeNumbers(): List[String] =
    phoneUserTable.filter(_.person.isEmpty).map(_.phone).toList

  def total(dateRange: DateRange): Double =
    operationJoinPrice.map(id  => (operationTable(id._1), priceTable(id._2)))
      .filter(id => dateRange.contains(id._1.date))
      .map(id => id._1.items * id._2.price).sum

  def lastCall(firstName: String, lastName: String): Option[Date] = {
    var result: Option[Date] = None
    for ((userId, operationId) <- phoneUserJoinOperation) {
      val user = phoneUserTable(userId)
      if (user.person.get.firstName == firstName && user.person.get.lastName == lastName) {
        val operation = operationTable(operationId)
        if (result.getOrElse(minDate) <= operation.date) {
          result = Some(operation.date)
        }
      }
    }
    result
  }

  private def joinAll(): Array[(Int, Int, Int)] = {
    val buffer = ArrayBuffer.empty[(Int, Int, Int)]
    assert(operationJoinPrice.length == phoneUserJoinOperation.length)
    for (i <- operationJoinPrice.indices) {
      assert(operationJoinPrice(i)._1 == phoneUserJoinOperation(i)._2 &&
        i == operationJoinPrice(i)._1)
      buffer.addOne((phoneUserJoinOperation(i)._1, i, operationJoinPrice(i)._2))
    }

    buffer.toArray
  }

  private def joinOperationPrice(): Array[(Int, Int)] = {
    val buffer = ArrayBuffer.empty[(Int, Int)]

    val typeToIndex = new mutable.HashMap[Operation, Int]()
    for (priceId <- priceTable.indices) {
      typeToIndex.addOne((priceTable(priceId).operationType, priceId))
    }

    for (operationId <- operationTable.indices) {
      val operationType = operationTable(operationId).operationType
      if (typeToIndex.contains(operationType)) {
        buffer.addOne(operationId, typeToIndex(operationType))
      } else {
        throw new DatabaseException(s"Operation $operationType is not found.")
      }
    }

    buffer.toArray
  }

  private def joinPhoneUserOperation(): Array[(Int, Int)] = {
    val buffer = ArrayBuffer.empty[(Int, Int)]

    val phoneToUserIndex = mutable.HashMap[String, Int]()
    for (phoneUserId <- phoneUserTable.indices) {
      if (phoneUserTable(phoneUserId).person.isDefined) {
        phoneToUserIndex.addOne((phoneUserTable(phoneUserId).phone, phoneUserId))
      }
    }

    for (operationId <- operationTable.indices) {
      val phone = operationTable(operationId).phone
      if (phoneToUserIndex.contains(phone)) {
        buffer.addOne((phoneToUserIndex(phone), operationId))
      } else {
        throw new DatabaseException(s"User with phone $phone is not defined.")
      }
    }

    buffer.toArray
  }

  private def loadPhoneUser(): Array[PhoneUserRow] = {
    val source = Source.fromFile(phoneUserFilename)
    val lines = source.getLines()
    val buffer = ArrayBuffer.empty[PhoneUserRow]

    while (lines.hasNext) {
      val line = lines.next
      val params = line.split(",")

      if (params.length != 3) {
        throw new DatabaseException(s"""Wrong format of PhoneUser.txt file.
             | Error occured in line \"$line\"""".stripMargin)
      }

      val person: Option[Person] = if (params(0) == "NULL" && params(1) == "NULL") {
        None
      } else {
        Some(Person(params(0), params(1)))
      }

      buffer.addOne(PhoneUserRow(person, params(2)))
    }
    source.close()

    buffer.toArray
  }

  private def operationByName(name: String): Operation = {
    try {
      Operation.withName(name)
    } catch {
      case _: Throwable  =>
        throw new DatabaseException(s"""Wrong format of operation type.
             | Error occured in symbol \\"$name\\""".stripMargin)
    }
  }

  private def loadOperation(): Array[OperationRow] = {
    val source = Source.fromFile(operationFilename)
    val lines = source.getLines()
    val buffer = ArrayBuffer.empty[OperationRow]

    while (lines.hasNext) {
      val line = lines.next
      val params = line.split(",")

      if (params.length != 5) {
        throw new DatabaseException(s"""Wrong format of Operation.txt file.
             | Error occured in line \\"$line\\""".stripMargin)
      }

      val datePattern = "([0-9]{2})\\.([0-9]{2})\\.([0-9]{4})".r
      val date = params(2) match {
        case datePattern(day, month, year) =>
          Date(day.toInt, month.toInt, year.toInt)
        case _ =>
          throw new DatabaseException(s"""Date must be in format DD.MM.YYYY.
               | ${params(2)} found.""".stripMargin)
      }

      val items = params(4).toIntOption.getOrElse({
        throw new DatabaseException(s"""Wrong operation items number.
             | Int expected, ${params(4)} found""".stripMargin)
      })

      buffer.addOne(OperationRow(params(0), operationByName(params(1)),
        date, params(3), items))
    }
    source.close()

    buffer.toArray
  }

  private def loadPrice(): Array[PriceRow] = {
    val source = Source.fromFile(priceFilename)
    val lines = source.getLines()
    val buffer = ArrayBuffer.empty[PriceRow]

    while (lines.hasNext) {
      val line = lines.next
      val params = line.split(",")

      if (params.length != 2) {
        throw new DatabaseException(s"""Wrong format of Price.txt file.
             | Error occured in line \\"$line\\""".stripMargin)
      }

      val price = params(1).toDoubleOption.getOrElse({
        throw new DatabaseException(s"""Wrong operation price format.
             | Double expected, ${params(1)} found""".stripMargin)
      })

      buffer.addOne(PriceRow(operationByName(params(0)), price))
    }
    source.close()

    buffer.toArray
  }
}
