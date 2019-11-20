package ru.spbau.jvm.scala

import java.time.{Duration, Instant, ZoneOffset}

case class Call(callerNumber: String, calleeNumber: String, time: Instant,
           duration: Duration, cost: Money) {
  override def toString: String = callerNumber + " | " +
                                  calleeNumber + " | " +
                                  time.atOffset(ZoneOffset.ofHours(3)) + " | " +
                                  duration.getSeconds + " | " +
                                  cost
}
