package ru.spbau.jvm.scala.cli

import java.text.SimpleDateFormat

import scala.util.{Failure, Success, Try}

class QueryParserFormatException(private val commandName: String) extends Exception {
  override def getMessage: String = s"malformed `$commandName` command"
}

class QueryParserCommandNotFoundException(private val commandName: String) extends Exception {
  override def getMessage: String = s"command `$commandName` not found"
}

object QueryParser {

  def parse(raw: String): Try[Query] = {
    val words = raw.trim().split("\\s+").toList
    words(0) match {
      case "avg" => QueryParserAvg(words.tail)
      case "calls" => QueryParserCalls(words.tail)
      case "messages" => QueryParserMessages(words.tail)
      case "contact" => QueryParserContent(words.tail)
      case "total" => QueryParserTotal(words.tail)
      case "number" => QueryParserNumber(words.tail)
      case "help" => Success(new HelpQuery)
      case _ => Failure(new QueryParserCommandNotFoundException(words(0)))
    }
  }

  object QueryParserAvg {
    def apply(tail: List[String]): Try[AvgQuery] = {
      tail.lift(0) match {
        case Some("cost") =>
          DateRangeParser(tail.tail) map { dateRange => new AvgCostQuery(dateRange) }
        case Some("duration") =>
          DateRangeParser(tail.tail) map { dateRange => new AvgDurationQuery(dateRange) }
        case _ =>
          DateRangeParser(tail) map { dateRange => new AvgDurationQuery(dateRange) }
      }
    }
  }

  object QueryParserCalls {
    def apply(tail: List[String]): Try[CallQuery] = DateRangeParser(tail) map {
      dateRange => new CallQuery(dateRange)
    }
  }

  object QueryParserMessages {
    def apply(tail: List[String]): Try[MessageQuery] = {
      tail match {
        case firstName1 :: lastName1 :: tail => DateRangeParser(tail) map { dateRange =>
          new MessageQuery(PersonName(firstName1, lastName1), dateRange)
        }
        case _ => Failure(new QueryParserFormatException("messages"))
      }
    }
  }

  object QueryParserNumber {
    def apply(tail: List[String]): Try[NumberQuery] = tail match {
      case firstName :: lastName :: Nil => Success(new NumberQuery(PersonName(firstName, lastName)))
      case _ => Failure(new QueryParserFormatException("number"))
    }
  }

  object QueryParserTotal {
    def apply(tail: List[String]): Try[TotalQuery] = DateRangeParser(tail) map {
      dateRange => new TotalQuery(dateRange)
    }
  }

  object QueryParserContent {
    def apply(tail: List[String]): Try[ContactQuery] = tail match {
      case personFn1 :: personLn1 :: "to" :: personFn2 :: personLn2 :: tail =>
        DateRangeParser(tail) map { dateRange =>
          new ContactQuery(PersonName(personFn1, personLn1), PersonName(personFn2, personLn2), dateRange)
        }
      case _ => Failure(new QueryParserFormatException("contact"))
    }
  }
}

object DateRangeParser {
  def apply(tail: List[String]): Try[DateRange] = {
    lazy val formatter = new SimpleDateFormat("yyyy-MM-dd")
    tail match {
      case Nil => Success(DateRange())
      case "from" :: dateFrom :: Nil =>
        Try(formatter.parse(dateFrom)) map { dateFrom =>
          DateRange(Some(dateFrom))
        }
      case "from" :: dateFrom :: "to" :: dateTo :: Nil =>
        Try(formatter.parse(dateFrom)) flatMap { dateFrom =>
          Try(formatter.parse(dateTo)) map { dateTo =>
            DateRange(Some(dateFrom), Some(dateTo))
          }
        }
      case _ => Failure(new QueryParserFormatException("date-range"))
    }
  }
}


