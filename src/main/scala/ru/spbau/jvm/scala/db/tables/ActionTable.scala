package ru.spbau.jvm.scala.db.tables

import scala.collection.mutable.ListBuffer

class ActionTable(title: List[String], body: ListBuffer[List[String]]) {
  private def filterRowByIds(row: List[String], ids: List[Int],
                             isRemove: Boolean => Boolean): List[String] = {
    row.indices.toArray.zip(row).toList
      .filter(i => isRemove(ids.contains(i._1)))
      .map(x => x._2)
  }

  private def filterTitleByAttrs(attrs: List[String],
                                 isRemove: Boolean => Boolean): List[String] = {
    title.filter(x => isRemove(attrs.contains(x)))
  }

  def getStr(): String = {
    title.mkString(" | ") + "\n" + body.map(row => row.mkString(" | ")).mkString("\n")
  }

  def getTitle(): List[String] = title

  def getBody(): ListBuffer[List[String]] = body

  def select(attr: String, predicate: String => Boolean): ActionTable = {
    new ActionTable(title, body.filter(row => predicate(row(title.indexOf(attr)))))
  }

  def filterCols(attrs: List[String],
                 isRemove: Boolean => Boolean): ActionTable = {
    val ids = attrs.map(i => title.indexOf(i))
    new ActionTable(filterTitleByAttrs(attrs, isRemove),
      body.map(row => filterRowByIds(row, ids, isRemove)))
  }

  def joinBy(table: ActionTable, attr: String): ActionTable = {
    val id = title.indexOf(attr)

    val titleRes = title:::table.filterTitleByAttrs(List(attr), x => !x)

    val bodyRes: ListBuffer[List[String]] = new ListBuffer[List[String]]()
    body.map(row => {
      bodyRes.addAll(
        table
          .select(attr, x => x.equals(row(id)))
          .filterCols(List(attr), x => !x)
          .getBody()
          .map(x => row:::x))
    })

    new ActionTable(titleRes, bodyRes)
  }

  def actionWithTwo(attr1: String, attr2: String,
                    newAttr: String,
                    function: (String, String) => String): ActionTable = {
    val id1 = title.indexOf(attr1)
    val id2 = title.indexOf(attr2)

    val titleRes = newAttr::title

    val bodyRes: ListBuffer[List[String]] = new ListBuffer[List[String]]()
    body.foreach(row => {
      bodyRes.addOne(function(row(id1), row(id2))::row)
    })

    new ActionTable(titleRes, bodyRes)
  }
}
