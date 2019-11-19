package ru.spbau.jvm.scala.db.rows

case class CallsRow(id: String, phoneNumberFrom: String, phoneNumberTo: String,
                    dateTime: String, duration: Double, cost: Double) extends Row
