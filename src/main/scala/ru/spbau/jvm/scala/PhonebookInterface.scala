package ru.spbau.jvm.scala

import java.time.LocalDateTime

import ru.spbau.jvm.scala.PhonebookSchema._
import slick.dbio.NoStream
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class PhonebookInterface(val database: Database) {
  def getTotal(from: LocalDateTime, to: LocalDateTime): Option[Int] =
    run(callsFromTo(from, to).map(_.cost).sum.result)

  def getAvg(from: LocalDateTime, to: LocalDateTime): Option[Int] =
    run(callsFromTo(from, to).map(_.cost).avg.result)

  def getCalls(from: LocalDateTime, to: LocalDateTime): Seq[((Int, String, String, Int), (Int, String, Int, Int, String))] =
    run(callsFromToWithUser(from, to).result)

  def getUserNumbers(name: String, surname: String): Seq[String] = run((
      for {
        user <- users if user.name === name && user.surname === surname
        number <- user.number
      } yield number.number
    ).result)

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
