package ru.spbau.jvm.scala.database

import java.io.File
import java.text.SimpleDateFormat

import ru.spbau.jvm.scala.csv.{SimpleCsv, SimpleCsvParser}
import ru.spbau.jvm.scala.utils.Utils

import scala.io.Source
import scala.util.{Failure, Success, Try}

/*
 * Parse a database from given files.
 *
 * Database is a list of CSV files. Each file is a table. Name of the table is its file name.
 * A header of a column in a table has format "<Field>(<type>)", where <Field> is an attribute name and <type> is an
 * attribute domain.
 */
object DbFileParser {
  def apply(paths: List[File]): Try[DbScheme] = {
    Utils.sequenceTry(paths map { fileName => parseDbName(fileName.getName) }) flatMap { tableNames =>
      Utils.sequenceTry(paths map parseDbFile) map { dbTables =>
        new DbScheme((tableNames zip dbTables).toMap)
      }
    }
  }

  private def parseDbFile(path: File): Try[DbTable] = {
    val file = Source.fromFile(path)
    val result = new SimpleCsvParser(',')
      .parse(file.getLines())
      .flatMap(csv => parseDbFileFromCsv(csv))
    file.close()
    result
  }

  private def parseDbFileFromCsv(csv: SimpleCsv): Try[DbTable] = {
    val dbHeadersT = csv.header.map(parseDbAttributeHeaderFromCsv)
    Utils.sequenceTry(dbHeadersT) flatMap { dbHeaders =>
      parseDbContent(dbHeaders, csv.content) map { dbContent =>
        new DbTable(
          DbTableHeader(dbHeaders),
          dbContent)
      }
    }
  }

  private def parseDbContent(dbHeaders: List[DbAttributeHeader], content: List[List[String]]): Try[DbTableContent] = {
    Utils.sequenceTry(content.map(parseDbTuple(dbHeaders, _))) map { dbContent =>
      new DbTableContent(dbContent)
    }
  }

  private def parseDbTuple(headers: List[DbAttributeHeader], row: List[String]): Try[DbTableTuple] =
    Utils.sequenceTry(row zip {
      headers
    } map { r => parseDbValue(r._1, r._2) }) map {
      list => DbTableTuple(list)
    }

  private def parseDbValue(value: String, header: DbAttributeHeader): Try[Any] =
    Try(header.domain match {
      case DbTypeInt => value.trim().toInt
      case DbTypeFloat => value.trim().toFloat
      case DbTypeString => value
      // Date format is *very* specific, but what else can I do?
      case DbTypeDate => new SimpleDateFormat("yyyy-MM-dd HH:mm").parse(value)
    })

  /*
   * Expected format: <name>(<type>)
   */
  private def parseDbAttributeHeaderFromCsv(raw: String): Try[DbAttributeHeader] = {
    val regex = """^(\w+)\((\w+)\)$""".r
    raw match {
      case regex(name, dbType) => Success(DbAttributeHeader(name, DbTypeMatcher(dbType)))
      case _ => Failure(new DbFileParserFormatException(raw))
    }
  }

  private def parseDbName(filename: String): Try[String] = {
    // Name of a table is a name of its file.
    val nameEnd = filename.indexOf(".txt")
    if (nameEnd <= 0)
      Failure(new DbFileParserMalformedNameException(filename))
    else
      Success(filename.substring(0, nameEnd))
  }
}
