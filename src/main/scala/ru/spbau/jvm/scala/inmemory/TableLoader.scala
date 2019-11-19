package ru.spbau.jvm.scala.inmemory

import java.time.LocalDateTime

import ru.spbau.jvm.scala.{Action, Operation, Phone, User}

object TableLoader {
  type TableWithId[T] = Map[Int, T]
  type Table[T] = Set[T]


  def tableUsers(iterator: Iterator[String]): TableWithId[User] =
    loadAbstractTableWithId(iterator, {
      case Seq(firstName, lastName) => User(firstName, lastName)
      case _ => throw new IllegalArgumentException("Parse error")
    })

  def tablePhones(iterator: Iterator[String]): TableWithId[Phone] =
    loadAbstractTableWithId(iterator, {
      case Seq(phone) => Phone(phone)
      case _ => throw new IllegalArgumentException("Parse error")
    })

  def tableActions(iterator: Iterator[String]): TableWithId[Action] =
    loadAbstractTableWithId(iterator, {
      case Seq(phoneId, operationId, date, count) => Action(phoneId.toInt, operationId.toInt, LocalDateTime.parse(date), count.toInt)
      case _ => throw new IllegalArgumentException("Parse error")
    })

  def tableUserPhones(iterator: Iterator[String]): Table[(Int, Int)] =
    loadAbstractTable(iterator, {
      case Seq(userId, phoneId) => (userId.toInt, phoneId.toInt)
      case _ => throw new IllegalArgumentException("Parse error")
    })

  def tableOperations(iterator: Iterator[String]): TableWithId[Operation] =
    loadAbstractTableWithId(iterator, {
      case Seq(description, cost) => Operation(description, cost.toFloat)
      case _ => throw new IllegalArgumentException("Parse error")
    })

  private def loadAbstractTable[A](iterator: Iterator[String], objectMapper: Seq[String] => A): Table[A] =
    iterator.map {
      row => {
        row.split(',').map(_.trim()) match {
          case Array(others@_*) => objectMapper(others)
          case _ => throw new IllegalArgumentException("Parse error")
        }
      }
    }.toSet

  private def loadAbstractTableWithId[A](iterator: Iterator[String], objectMapper: Seq[String] => A): TableWithId[A] =
    iterator.map {
      row => {
        row.split(',').map(_.trim()) match {
          case Array(id, others@_*) => (id.toInt, objectMapper(others))
          case _ => throw new IllegalArgumentException("Parse error")
        }
      }
    }.toMap
}
