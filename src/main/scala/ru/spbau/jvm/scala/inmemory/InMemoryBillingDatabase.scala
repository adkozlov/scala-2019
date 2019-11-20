package ru.spbau.jvm.scala.inmemory

import java.time.LocalDateTime

import ru.spbau.jvm.scala._
import ru.spbau.jvm.scala.inmemory.TableLoader._

import scala.io.Source
import scala.util.Using

class InMemoryBillingDatabase(val users: TableWithId[User],
                              val actions: TableWithId[Action],
                              val phones: TableWithId[Phone],
                              val userPhones: Table[(Int, Int)],
                              val operations: TableWithId[Operation]) extends BillingDatabase {

  def this(databaseDir: String) =
    this(
      users = Using(Source.fromFile(databaseDir + "/users.csv")) { f => TableLoader.tableUsers(f.getLines()) }.get,
      actions = Using(Source.fromFile(databaseDir + "/actions.csv")) { f => TableLoader.tableActions(f.getLines()) }.get,
      phones = Using(Source.fromFile(databaseDir + "/phones.csv")) { f => TableLoader.tablePhones(f.getLines()) }.get,
      userPhones = Using(Source.fromFile(databaseDir + "/userphones.csv")) { f => TableLoader.tableUserPhones(f.getLines()) }.get,
      operations = Using(Source.fromFile(databaseDir + "/operations.csv")) { f => TableLoader.tableOperations(f.getLines()) }.get
    )

  private def actionsInInterval(operationsIds: Set[Int], from: LocalDateTime, to: LocalDateTime): TableWithId[Action] = {
    actions.filter { case (_, action) =>
      operationsIds.contains(action.operationId) && action.date.isAfter(from) && action.date.isBefore(to)
    }
  }

  def callsInInterval(from: LocalDateTime, to: LocalDateTime): Iterable[(Option[User], Option[Phone], Int, Option[Float])] = {
    val calls = actionsInInterval(Set(0, 1), from, to)
    calls.values.map { action: Action =>
      val userId = userPhones.find(_._2 == action.phoneId).map(_._1)
      (
        userId.flatMap(users.get),
        phones.get(action.phoneId),
        action.count,
        operations.get(action.operationId).map(op => op.cost * action.count))
    }
  }

  def avgCallsDuration: Double = {
    val calls = actionsInInterval(Set(0, 1), LocalDateTime.MIN, LocalDateTime.MAX)
    calls.values.map { action: Action => action.count }
      .foldLeft((0.0, 1))((acc, i) => (acc._1 + (i - acc._1) / acc._2, acc._2 + 1))._1
  }

  def sumCost(from: LocalDateTime, to: LocalDateTime): Double = {
    val allActions = actionsInInterval(operations.keySet, from, to)
    allActions.values.map { action: Action => action.count * operations(action.operationId).cost }.iterator.sum
  }

  def userPhone(user: User): Option[Iterable[Phone]] =
    users.find(_._2 == user)
      .map { case (userId, _) => userPhones
        .filter(_._1 == userId).map(_._2).map(phoneId => phones.get(phoneId))
        .filter(phone => phone.nonEmpty).map(phone => phone.get)
      }

  def userActions(user: User, from: LocalDateTime, to: LocalDateTime): Option[Iterable[Action]] =
    users.find(_._2 == user)
      .map { case (userId, _) => userPhones.filter(_._1 == userId).map(_._2) }
      .map { phoneIds => actions.values.filter(a => phoneIds.contains(a.phoneId)) }

}
