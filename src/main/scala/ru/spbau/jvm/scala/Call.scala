package ru.spbau.jvm.scala

import java.time.LocalDate

case class Call(date: LocalDate, caller: Phone, callee: Phone, duration: Int, cost: Int)