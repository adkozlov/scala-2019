package ru.spbau.jvm.scala.company_entity

case class User(firstName: String, lastName: String) {
  override def toString: String = s"\'$firstName $lastName\'"
}
