package ru.spbau.jvm.scala.cli

import org.scalatest.FlatSpec

class QueryParserTest extends FlatSpec {
  "Parsing of avg cost" should "pass" in {
    assert(QueryParser.parse("avg cost").isSuccess)
    assert(QueryParser.parse("avg cost from 2019-11-01").isSuccess)
    assert(QueryParser.parse("avg cost from 2019-11-01 to 2019-11-02").isSuccess)
  }

  "Parsing of avg duration" should "pass" in {
    assert(QueryParser.parse("avg duration").isSuccess)
    assert(QueryParser.parse("avg duration from 2019-11-01").isSuccess)
    assert(QueryParser.parse("avg duration from 2019-11-01 to 2019-11-02").isSuccess)
  }

  "Parsing of calls" should "pass" in {
    assert(QueryParser.parse("calls").isSuccess)
    assert(QueryParser.parse("calls from 2019-11-01").isSuccess)
    assert(QueryParser.parse("calls from 2019-11-01 to 2019-11-02").isSuccess)
  }

  "Parsing of total" should "pass" in {
    assert(QueryParser.parse("total").isSuccess)
    assert(QueryParser.parse("total from 2019-11-01").isSuccess)
    assert(QueryParser.parse("total from 2019-11-01 to 2019-11-02").isSuccess)
  }

  "Parsing of contact" should "pass" in {
    assert(QueryParser.parse("contact A B to J H").isSuccess)
    assert(QueryParser.parse("contact A B to J H from 2019-11-01").isSuccess)
    assert(QueryParser.parse("contact A B to J H from 2019-11-01 to 2019-11-02").isSuccess)
  }


  "Parsing of messages" should "pass" in {
    assert(QueryParser.parse("messages A B").isSuccess)
    assert(QueryParser.parse("messages A B from 2019-11-01").isSuccess)
    assert(QueryParser.parse("messages A B from 2019-11-01 to 2019-11-02").isSuccess)
  }

  "Parsing of help" should "pass" in {
    assert(QueryParser.parse("help").isSuccess)
  }

  "Parsing of avg cost" should "fail" in {
    assert(QueryParser.parse("avg cost ost").isFailure)
    assert(QueryParser.parse("avg cost from oop").isFailure)
    assert(QueryParser.parse("avg cost from 2019-11-01 until 2019-11-02").isFailure)
  }

  "Parsing of avg duration" should "fail" in {
    assert(QueryParser.parse("avg durotion").isFailure)
    assert(QueryParser.parse("avg duration from -11-01").isFailure)
    assert(QueryParser.parse("avg duration from 2019-11-01 to 19lll11-02").isFailure)
  }

  "Parsing of calls" should "fail" in {
    assert(QueryParser.parse("calls shmalls").isFailure)
    assert(QueryParser.parse("calls  2019-11-01").isFailure)
    assert(QueryParser.parse("calls from 2019-11-01 2019-11-02").isFailure)
  }

  "Parsing of total" should "fail" in {
    assert(QueryParser.parse("total A").isFailure)
    assert(QueryParser.parse("total from -11-").isFailure)
    assert(QueryParser.parse("total from 2019-11-01 upto you").isFailure)
  }

  "Parsing of contact" should "fail" in {
    assert(QueryParser.parse("contact A to J H").isFailure)
    assert(QueryParser.parse("contact A B to J from 2019-11-01").isFailure)
    assert(QueryParser.parse("contact A B to J H to 2019-11-02").isFailure)
  }


  "Parsing of messages" should "fail" in {
    assert(QueryParser.parse("messages A").isFailure)
    assert(QueryParser.parse("messages A B 2019-11-01").isFailure)
    assert(QueryParser.parse("messages A B from 2019-11-01 to").isFailure)
  }

  "Parsing of help" should "fail nevertheless" in {
    assert(QueryParser.parse("help who cares about tail words in the help commands?").isSuccess)
  }


}
