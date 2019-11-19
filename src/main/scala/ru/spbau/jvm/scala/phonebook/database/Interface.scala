package ru.spbau.jvm.scala.phonebook.database

import java.time.LocalDateTime

import ru.spbau.jvm.scala.phonebook.database.PhonebookSchema._
import slick.dbio.NoStream
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class Interface(val database: Database) {
  def getTotal(from: LocalDateTime, to: LocalDateTime): Option[Int] =
    run(callsFromTo(from, to).map(_.cost).sum.result)

  def getAvg(from: LocalDateTime, to: LocalDateTime): Option[Int] =
    run(callsFromTo(from, to).map(_.cost).avg.result)

  def getCalls(from: LocalDateTime, to: LocalDateTime): Seq[((Int, String, String, Int), (Int, String, Int, Int, String))] =
    run(callsFromToWithUser(from, to).result)

  def getNumberUsers(number: String): Seq[(String, String)] =
    run((
      for {
        user <- users
        num <- user.number if num.number === number
      } yield (user.name, user.surname)
      ).result)

  def getInternalCalls(from: LocalDateTime, to: LocalDateTime): Seq[((Int, String, String, Int), (Int, String, Int, Int, String))] = run((
    for {
      num <- numbers
      (user, call) <- callsFromToWithUser(from, to) if call.callee === num.number
    } yield (user, call)
  ).result)

  def getUsersCalledTo(callee: String): Seq[(String, String)] = run((
    for {
      call <- calls if call.callee === callee
      user <- call.user
    } yield (user.name, user.surname)
    ).distinct.result)

  def getUserNumbersLeftJoined(name: String, surname: String): Option[Seq[String]] = {
    def optionalNumbers =
      (users.filter { u => u.name === name && u.surname === surname }.joinLeft(numbers) on (_.number_id === _.id))
      .map(_._2.map(_.number))

    // for each user with provided name and surname contains None or each number of this user
    val numbersResult: Seq[Option[String]] = run(optionalNumbers.result)


    if (numbersResult.isEmpty)
      // no users found
      None
    else
      Some(numbersResult.flatten)
  }

  private def callsFromTo(from: LocalDateTime, to: LocalDateTime) =
    calls.filter(
      c => c.datetime >= dateTimeToString(from)
        && c.datetime < dateTimeToString(to)
    )

  private def callsFromToWithUser(from: LocalDateTime, to: LocalDateTime) =
    for {
      call <- callsFromTo(from, to)
      usr <- call.user
    } yield (usr, call)

  /**
    * converts java representation of datetime to sql representation
    */
  private def dateTimeToString(dateTime: LocalDateTime) = dateTime.toString.replace("T", " ")

  private def run[R](action: DBIOAction[R, NoStream, Nothing]) = Await.result(database.run(action), Duration.Inf)
}
