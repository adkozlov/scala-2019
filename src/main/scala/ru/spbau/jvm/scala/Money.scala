package ru.spbau.jvm.scala

class Money(val cents: Long) {

  override def toString: String = {
    val dollarsPart = cents / 100
    val centsPart = cents % 100
    val centsString = if (centsPart < 10) {
      "0" + centsPart
    } else {
      centsPart.toString
    }
    dollarsPart + "." + centsString
  }

}
