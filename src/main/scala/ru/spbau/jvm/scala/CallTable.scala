package ru.spbau.jvm.scala

import java.time.{Duration, LocalDateTime, ZoneOffset}

import scala.io.Source

final class CallTable(calls: List[Call]) {
  def all: List[Call] = calls

  def getCallsTimeInterval(from: LocalDateTime, to: LocalDateTime): List[Call] = {
    val fromInstant = from.toInstant(ZoneOffset.ofHours(3))
    val toInstant = to.toInstant(ZoneOffset.ofHours(3))
    calls.filter(call => call.time.isAfter(fromInstant) && call.time.isBefore(toInstant))
  }

  def getCallsFromTime(from: LocalDateTime): List[Call] = {
    val fromInstant = from.toInstant(ZoneOffset.ofHours(3))
    calls.filter(call => call.time.isAfter(fromInstant))
  }

  def getCallsBeforeTime(before: LocalDateTime): List[Call] = {
    val beforeInstant = before.toInstant(ZoneOffset.ofHours(3))
    calls.filter(call => call.time.isBefore(beforeInstant))
  }
}

object CallTable {
  def apply(): CallTable = {
    val tableSource = Source.fromFile("resources/calls.csv")
    val calls = tableSource.getLines().drop(1)
      .map(_.split(","))
      .map(callFromRecord)
    new CallTable(calls.toList)
  }

  private def parsePrice(price: String): Long = {
    val pointPosition = price.indexOf('.')
    if (pointPosition == -1) {
      price.toLong * 100
    } else {
      price.replaceAll("\\.", "").toLong
    }
  }

  private def callFromRecord(record: Array[String]): Call = {
    val callerNumberString = record(0)
    val calleeNumberString = record(1)
    val timeString = record(2)
    val durationString = record(3)
    val priceString = record(4)
    val callerNumber = callerNumberString.replaceAll("[^\\d]", "")
    val calleeNumber = calleeNumberString.replaceAll("[^\\d]", "")
    val localTime = LocalDateTime.parse(timeString)
    val time = localTime.toInstant(ZoneOffset.ofHours(3))
    val duration = Duration.ofSeconds(durationString.toLong)
    val price = parsePrice(priceString)
    Call(callerNumber, calleeNumber, time, duration, new Money(price))
  }
}
