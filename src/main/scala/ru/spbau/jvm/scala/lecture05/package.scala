package ru.spbau.jvm.scala

package object lecture05 {

  type IntToString = GenericMap[Int, String] // type alias

  implicit class StringExt(private val string: String) extends AnyVal {

    def concat(other: String): String =
      string + other
  }

  def concat(left: String, right: String): String =
    left + right
}
