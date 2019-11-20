package ru.spbau.jvm.scala.database

import ru.spbau.jvm.scala.utils.Utils

import scala.util.{Failure, Success, Try}

object DbTypeMatcher {
  def apply(typ: String): DbType = typ match {
    case "string" => DbTypeString
    case "int" => DbTypeInt
    case "float" => DbTypeFloat
    case "date" => DbTypeDate
    case _ => throw new DbFileParserFormatException(typ)
  }
}

case class DbAttributeHeader(name: String, domain: DbType)

case class DbTableHeader(attributeHeaders: List[DbAttributeHeader]) {
  def indexOf(column: String): Int = indexOfZipped(attributeHeaders.zipWithIndex, column)

  private def indexOfZipped(zipped: List[(DbAttributeHeader, Int)], name: String): Int =
    zipped find { p => p._1.name == name } match {
      case Some(tup) => tup._2
      case None => throw new DbNoSuchColumnException(name)
    }

  def indicesOf(columns: List[String]): List[Int] = {
    val zipped = attributeHeaders.zipWithIndex
    columns map {
      indexOfZipped(zipped, _)
    }
  }

  def merge(tableHeader: DbTableHeader, column: String): DbTableHeader = {
    val newAttrHeader = tableHeader.attributeHeaders filterNot { attHeader => attHeader.name == column }
    DbTableHeader(attributeHeaders ++ newAttrHeader)
  }

  override def toString: String = attributeHeaders.map(_.name).mkString(" ")
}

case class DbTableTuple(tuple: List[Any]) {
  def get(idx: Int): Option[Any] = tuple.lift(idx)

  /*
   * Create a new tuple from this and `tableTuple`, excluding a field by `idx` index from the given tuple.
   */
  def merge(tableTuple: DbTableTuple, idx: Int): DbTableTuple = {
    val (head, _ :: tail) = tableTuple.tuple.splitAt(idx)
    DbTableTuple(tuple ++ head ++ tail)
  }

  override def toString: String = tuple.mkString(" ")
}

class DbTableContent(private val content: List[DbTableTuple]) {
  def column(colIdx: Int): Option[List[Any]] = Utils.sequenceOption(content.map(_.tuple.lift(colIdx)))

  def tuples(): Iterator[DbTableTuple] = content.iterator

  override def toString: String = content.mkString("\n")
}

class DbTable(val dbTableHeader: DbTableHeader,
              val dbTableContent: DbTableContent) {
  def column(name: String): List[Any] = {
    val colIdx = columnIndex(name)
    columnByIndex(colIdx) match {
      case Some(column) => column
      case None => throw new DbNoSuchColumnException(name)
    }
  }

  def columnIndex(name: String): Int = dbTableHeader.indexOf(name)

  def columnByIndex(idx: Int): Option[List[Any]] = dbTableContent.column(idx)

  def selectByNames(names: List[String]): Iterator[DbTableTuple] = {
    val ids = dbTableHeader.indicesOf(names)
    dbTableContent.tuples() map { row =>
      DbTableTuple(ids collect row.tuple)
    }
  }

  def select[T](column: String, value: T): DbTable = selectWhere(column, (field: T) => field == value)

  def selectWhere[T](column: String, pred: T => Boolean): DbTable = {
    val idx = columnIndex(column)
    val content = (tuples() filter { tuple => pred.apply(tuple.get(idx).get.asInstanceOf[T]) }).toList
    if (content.isEmpty)
      throw new DbSelectException(column)
    else
      new DbTable(dbTableHeader, new DbTableContent(content))
  }

  def selectWhereBi[T1, T2](column1: String, column2: String, biPred: (T1, T2) => Boolean): DbTable = {
    val idx1 = columnIndex(column1)
    val idx2 = columnIndex(column2)
    val contentI = tuples() filter { row =>
      biPred.apply(row.get(idx1).get.asInstanceOf[T1], row.get(idx2).get.asInstanceOf[T2])
    }
    val content = contentI.toList
    if (content.isEmpty)
      throw new DbSelectException(column1)
    else
      new DbTable(dbTableHeader, new DbTableContent(content))
  }

  def tuples(): Iterator[DbTableTuple] = dbTableContent.tuples()

  def foldColumns[T1, T2, T3](column1: String, column2: String,
                              destAttributeHeader: DbAttributeHeader,
                              op: (T1, T2) => T3): DbTable = {
    val idx1 = columnIndex(column1)
    val idx2 = columnIndex(column2)
    val content = tuples() map { row =>
      val res = op.apply(row.get(idx1).get.asInstanceOf[T1], row.get(idx2).get.asInstanceOf[T2])
      DbTableTuple(res :: row.tuple)
    }
    new DbTable(
      DbTableHeader(destAttributeHeader :: dbTableHeader.attributeHeaders),
      new DbTableContent(content.toList)
    )
  }

  def removeColumn(name: String): DbTable = Try(columnIndex(name)) match {
    case Failure(_) => new DbTable(dbTableHeader, dbTableContent)
    case Success(idx) =>
      val content = tuples() map {
        row =>
          val (head, _ :: tail) = row.tuple.splitAt(idx)
          DbTableTuple(head ++ tail)
      }
      val (head, _ :: tail) = dbTableHeader.attributeHeaders.splitAt(idx)
      new DbTable(DbTableHeader(head ++ tail), new DbTableContent(content.toList))
  }

  override def toString: String = dbTableHeader.toString + "\n" + dbTableContent.toString
}

class DbScheme(private val scheme: Map[String, DbTable]) {
  def joinOwn(tableName1: String, tableName2: String, column1: String, column2: String): DbTable =
    join(table(tableName1), table(tableName2), column1, column2)

  def table(tableName: String): DbTable = scheme.get(tableName) match {
    case None => throw new DbNoSuchTableException(tableName)
    case Some(table) => table
  }

  def join(table1: DbTable, table2: DbTable, column1: String, column2: String): DbTable = {
    val colIdx1 = table1.columnIndex(column1)
    val colIdx2 = table2.columnIndex(column2)

    val newHeader = table1.dbTableHeader.merge(table2.dbTableHeader, column2)
    val newContent = table1.tuples() flatMap { tuple1 =>
      table2.tuples() filter {
        tuple1.get(colIdx1) == _.get(colIdx2)
      } map {
        tuple1.merge(_, colIdx2)
      }
    }
    new DbTable(newHeader, new DbTableContent(newContent.toList))
  }
}