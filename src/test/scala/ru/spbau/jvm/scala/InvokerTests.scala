package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.cli.Invoker
import ru.spbau.jvm.scala.cli.commands.{AvgCallDuration, CallsFromDateTimeRange, PhoneNumberOfEmployee}
import ru.spbau.jvm.scala.db.DB

class InvokerTests extends FlatSpec with Matchers {
  val db = new DB()
  db.loadTables("resources")
  val invoker = new Invoker(db)

  Map("CallsFromDateTimeRange" -> new CallsFromDateTimeRange,
    "AvgCallDuration" -> new AvgCallDuration,
    "PhoneNumberOfEmployee" -> new PhoneNumberOfEmployee)
    .foreach(info => invoker.register(info._1, info._2))

  "CallsFromDateTimeRange" should "{Cost($) | Duration(s) | Callee | Name | LastName | Description}" in {
    val res: String = invoker.exec("CallsFromDateTimeRange",
      Array("2019-09-16T12:53:50+02:00", "2019-11-16T12:53:51+02:00"))
    res should be(List(
      "Cost($) | Duration(s) | Callee | Name | LastName | Description",
      "1.0 | 59 | 85974701195 | Leta | Lowman | out_call",
      "0.0 | 640 | 85088708130 | Ayako | Stelly | in_call",
      "1.0 | 18 | 85088708130 | Mercedes | Schnur | out_call",
      "0.0 | 219 | 89295452814 | Leta | Lowman | in_call",
      "9.0 | 515 | 85088708130 | Hortensia | Markowitz | out_call",
      "0.0 | 902 | 82479931957 | Leta | Lowman | in_call").mkString("\n"))
  }

  "AvgCallDuration" should "{Avg(s)}" in {
    val res: String = invoker.exec("AvgCallDuration", Array())
    res should be(List(
      "Avg(s)",
      "261"
    ).mkString("\n"))
  }

  "PhoneNumberOfEmployee" should "{Name | LastName | PhoneNumber}" in {
    val res: String = invoker.exec("PhoneNumberOfEmployee",
      Array("Leta", "Lowman"))
    res should be(List(
      "Name | LastName | PhoneNumber",
      "Leta | Lowman | 85088708130"
    ).mkString("\n"))
  }
}
