package ru.spbau.jvm.scala

import java.io.StringWriter

import org.scalatest.{FlatSpec, FunSuite, Matchers}

class DatabaseExecutorTest extends FlatSpec with Matchers {

  val database = new Database("src/test/resources/PhoneUser.txt",
    "src/test/resources/Operation.txt",
    "src/test/resources/Price.txt")

  "executor" should "write error message on unexpected command" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "temp", stringWriter)
    stringWriter.toString should be ("command 'temp' not found\n")
  }

  "executor" should "write error message on unexpected employee" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "number William Shakespeare", stringWriter)
    stringWriter.toString should be ("employee 'William Shakespeare' not found\n")
  }

  "executor" should "parse total without parameters" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "total", stringWriter)
    stringWriter.toString should be ("5309.800000000001\n")
  }

  "executor" should "parse total without TO date" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "total from 15.11.2019", stringWriter)
    stringWriter.toString should be ("4998.6\n")
  }

  "executor" should "parse calls without parameters" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "calls", stringWriter)
    stringWriter.toString should be ("""FirstName | LastName | Callee | Duration (s) | Cost ($)
                                       |Aba | Caba | 8 800 555 35 35 | 42 | 84.0
                                       |Aba | Caba | 8 800 555 35 35 | 1 | 2.0
                                       |Aba | Caba | 8 800 555 35 35 | 2 | 4.0
                                       |Aba | Caba | 8 800 555 35 35 | 3 | 6.0
                                       |Alice | ALice | 8 800 555 35 35 | 42 | 84.0
                                       |Alice | ALice | 8 800 555 35 35 | 3 | 6.0
                                       |Alice | ALice | 8 800 555 35 35 | 1 | 2.0
                                       |Alice | ALice | 8 800 555 35 35 | 42 | 84.0
                                       |Bob | Bob | 9090909090909 | 239 | 478.0
                                       |Aba | Caba | 8 800 555 35 35 | 4 | 8.0
                                       |""".stripMargin)
  }

  "executor" should "parse calls without TO date" in {
    val stringWriter = new StringWriter()
    DatabaseExecutor.execute(database, "calls from 16.11.2019", stringWriter)
    stringWriter.toString should be ("""FirstName | LastName | Callee | Duration (s) | Cost ($)
                                       |Aba | Caba | 8 800 555 35 35 | 42 | 84.0
                                       |Alice | ALice | 8 800 555 35 35 | 42 | 84.0
                                       |Alice | ALice | 8 800 555 35 35 | 1 | 2.0
                                       |Bob | Bob | 9090909090909 | 239 | 478.0
                                       |""".stripMargin)
  }

}
