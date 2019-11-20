package ru.spbau.jvm.scala.csv

class SimpleCsvParserFormatException(private val index: Int) extends Exception {
  override def getMessage: String = s"$index: mismatch row size"
}


