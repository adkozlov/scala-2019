package ru.spbau.jvm.scala.csv

import org.scalatest.FlatSpec


class SimpleCsvParserTest extends FlatSpec {
  "Simple test" should "parse well!" in {
    val csv =
      """Field1,Field2,Field3
        |1,a,c
        |2,f,d
        |4,a,s
        |""".stripMargin.split('\n').iterator

    val parser = new SimpleCsvParser(',')
    val result = parser.parse(csv)
    assert(result.isSuccess)
    val parsedCsv = result.get
    assert(parsedCsv.header == List("Field1", "Field2", "Field3"))
    assert(parsedCsv.content == List(List("1", "a", "c"), List("2", "f", "d"), List("4", "a", "s")))
  }

  "Simple test with other separator" should "parse well!" in {
    val csv =
      """Field1;Field2;Field3
        |1;a;c
        |2;f;d
        |4;a;s
        |""".stripMargin.split('\n').iterator

    val result = new SimpleCsvParser(';').parse(csv)
    assert(result.isSuccess)
    val parsedCsv = result.get
    assert(parsedCsv.header == List("Field1", "Field2", "Field3"))
    assert(parsedCsv.content == List(List("1", "a", "c"), List("2", "f", "d"), List("4", "a", "s")))
  }

  "Parsing of malformed tables" should "fail!" in {
    val csv =
      """Field1;Field2;Field3
        |1;a
        |2
        |4;a;s
        |""".stripMargin.split('\n').iterator
    val result = new SimpleCsvParser(';').parse(csv)
    assert(result.isFailure)
  }
}
