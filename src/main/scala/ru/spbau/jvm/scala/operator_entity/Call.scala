package ru.spbau.jvm.scala.operator_entity

import java.util.Date

case class Call(callee: String,
                date: Date,
                duration: Int,
                cost: Double)
