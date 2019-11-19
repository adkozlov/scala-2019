package ru.spbau.jvm.scala.db.tables

import scala.collection.mutable.ListBuffer

class ActionTable(title: List[String], body: ListBuffer[List[String]]) {
  private def filterRowByIds(row: List[String], ids: List[Int]): List[String] = {
    row.indices.toArray.zip(row).toList
      .filter(i => !ids.contains(i._1))
      .map(x => x._2)
  }

  private def filterTitleByAttrs(attrs: List[String]): List[String] = {
    title.filter(x => !attrs.contains(x))
  }

  def getStr(): String = {
    title.mkString(" | ") + "\n" + body.map(row => row.mkString(" | ")).mkString("\n")
  }

  def getTitle(): List[String] = title

  def getBody(): ListBuffer[List[String]] = body

  def select(attr: String, predicate: String => Boolean): ActionTable = {
    new ActionTable(title, body.filter(row => predicate(row(title.indexOf(attr)))))
  }

  def filterCols(attrs: List[String]): ActionTable = {
    val ids = attrs.map(i => title.indexOf(i))
    new ActionTable(filterTitleByAttrs(attrs), body.map(row => filterRowByIds(row, ids)))
  }

  def joinBy(table: ActionTable, attr: String): ActionTable = {
    val id = title.indexOf(attr)

    val titleRes = title:::table.filterTitleByAttrs(List(attr))

    val bodyRes: ListBuffer[List[String]] = new ListBuffer[List[String]]()
    body.map(row => {
      bodyRes.addAll(table.select(attr, x => x.equals(row(id))).filterCols(List(attr)).getBody().map(x => row:::x))
    })

    new ActionTable(titleRes, bodyRes)
  }
}
