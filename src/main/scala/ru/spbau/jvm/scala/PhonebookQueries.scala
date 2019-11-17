package ru.spbau.jvm.scala

import java.time.LocalDateTime

import slick.jdbc.SQLiteProfile.api._

object PhonebookQueries {
  import PhonebookSchema._

  def selectAllUsers = users.map(p => (p.id, p.name, p.number_id))

  def callsFromTo(from: LocalDateTime, to: LocalDateTime = LocalDateTime.now()) =
    calls.filter(
      c => c.datetime >= dateTimeToString(from)
        && c.datetime < dateTimeToString(to)
    )

  def costFromTo(from: LocalDateTime, to: LocalDateTime = LocalDateTime.now()) =
    callsFromTo(from, to).map(_.cost).sum

  def avg = calls.map(_.cost).avg

  def userNumberLeftJoin = users.joinLeft(numbers) on (_.number_id === _.id)

  def userNumberJoin = users.flatMap{ user =>
    user.number.map {
      num => (user.name, user.surname, num.number)
    }
  }

  def userNumberQuery(name: String, surname: String) = for {
      usr <- users if usr.name === name && usr.surname === surname
      num <- numbers if num.id === usr.number_id
    } yield num.number

  def userNumberQuery2(name: String, surname: String) = for {
    (user, num) <- userNumberLeftJoin if user.name === name && user.surname === surname
  } yield num.map(_.number)

  // converts java representation of datetime to sql representation
  private def dateTimeToString(dateTime: LocalDateTime) = dateTime.toString.replace("T", " ")
}
