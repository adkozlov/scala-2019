package ru.spbau.jvm.scala

import org.joda.time.DateTime

case class Call(caller: Phone,
                callee: Phone,
                date: DateTime,
                duration: Int,
                cost: Float)
