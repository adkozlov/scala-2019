package ru.spbau.jvm.scala

case class Employee(firstName: String, lastName: String) {
  override def toString: String = firstName + " " + lastName
}