package ru.spbau.jvm.scala

import java.text.SimpleDateFormat

import Main._

import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

class MainSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    loadEmployees()
    loadCalls()
  }

  private val format = new SimpleDateFormat("dd.MM.yyyy")

  "calls" should "return all calls" in {
    getCalls(format.parse("01.01.0000"), format.parse("31.12.9999")).length should be (11)
  }

  "avg" should "return correct value" in {
    getAvg should be (218)
  }

  "total" should "return correct value" in {
    getTotal() should be (37)
    getTotal(from = format.parse("01.02.2020")) should be (17)
    getTotal(to = format.parse("01.01.2020")) should be (9)
    getTotal(from = format.parse("25.01.2020"), to = format.parse("27.01.2020")) should be (5)
  }

  "total by" should "return correct value" in {
    getTotalBy("Donald", "Sanchez") should be (0)
    getTotalBy("Rose", "Reed") should be (3)
    getTotalBy("Emily", "Reynolds") should be (11)
  }

  "max total" should "return correct value" in {
    getMaxTotal should be ("Emily Reynolds")
  }

  "number" should "return correct value" in {
    getNumber("Nancy", "Bradley") should be ("+70000000003")
    getNumber("Tiffany", "Cook") should be ("+70000000004")
    getNumber("Matthew", "McDonald") should be ("+70000000005")
  }

  "name" should "return correct value" in {
    getName("+70000000006") should be ("James Berry")
    getName("+70000000007") should be ("Stephen Daniels")
    getName("+70000000008") should be ("Debra Martinez")
  }

}
