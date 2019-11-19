import java.time.{Duration, LocalDateTime}

import org.scalatest.FunSuite
import java.time.Duration
import ru.spbau.jvm.scala.phonebook.Main._

class MainTest extends FunSuite {

  test("afterMatch returns correct substring") {
    assert("qwe" == afterMatch("aaaqwe", "aaa".r).get)
  }
  test("afterMatch skips spaces when needed") {
    assert("qwe" == afterMatch("aaa   qwe", "aaa[\\s]*".r).get)
  }

  val dateTime: LocalDateTime = LocalDateTime.of(1999, 7, 30, 10, 11, 12, 123)
  val onlyDate = dateTime.toLocalDate
  test("parseDate parses date with time correctly") {
    assert(dateTime == parseDate("kek " + dateTime.toString, "kek").get)
  }
  test("parseDate parses date without time correctly") {
    assert(onlyDate.atStartOfDay() == parseDate("kek " + onlyDate.toString, "kek").get)
  }
  test("parseDate modifies date without time correctly") {
    val someDateTime = LocalDateTime.of(1,1,1,1,1)
    assert(someDateTime == parseDate("kek " + onlyDate.toString, "kek", _ => someDateTime).get)
  }

  test("parseDate gets date not only from string beginning") {
    val someDateTime = LocalDateTime.of(1,1,1,1,1)
    assert(someDateTime == parseDate("some other text kek " + onlyDate.toString, "kek", _ => someDateTime).get)
  }
  test("parseDate gets date preceded by some spaces of different kinds") {
    val someDateTime = LocalDateTime.of(1,1,1,1,1)
    assert(someDateTime == parseDate("Here come spaces and tabs: kek       " + onlyDate.toString, "kek", _ => someDateTime).get)
  }

  test("parseDates basic test") {
    val otherDateTime = dateTime.plusMinutes(1234)
    val parsed = parseDates(s"from $dateTime to $otherDateTime")
    assert(dateTime == parsed._1)
    assert(otherDateTime == parsed._2)
  }
  test("parseDates returns default values") {
    val parsed = parseDates(s"sorry no dates here")
    assert(LocalDateTime.MIN == parsed._1)
    assert(Duration.between(parsed._2, LocalDateTime.now()).abs().toMinutes < 1,
      "second argument is less than one minute away from now")
  }
}

// a workaround for Idea Scala plugin error
object Runner extends App {
  org.scalatest.run(new MainTest())
}