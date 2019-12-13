package ru.spbau.jvm.scala

import java.time.LocalDateTime

case class Operation(description: String, cost: Float)

case class User(firstName: String, lastName: String)

case class Phone(value: String)

case class Action(phoneId: Int, operationId: Int, date: LocalDateTime, count: Int)

trait BillingDatabase {
  def callsInInterval(from: LocalDateTime, to: LocalDateTime): Iterable[(Option[User], Option[Phone], Int, Option[Float])]

  def avgCallsDuration: Double

  def sumCost(from: LocalDateTime, to: LocalDateTime): Double

  def userPhone(user: User): Option[Iterable[Phone]]

  def userActions(user: User, from: LocalDateTime, to: LocalDateTime): Option[Iterable[Action]]

  def registeredUsers: Iterable[User]

  def userActionsCost(user: User, from: LocalDateTime, to: LocalDateTime): Option[Double]
}
