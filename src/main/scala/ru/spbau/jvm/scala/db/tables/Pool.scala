package ru.spbau.jvm.scala.db.tables

import java.io.File

import ru.spbau.jvm.scala.db.tables.rows.PoolRow

class Pool extends Table[PoolRow] {
  override def load(file: File): Unit  = {
    super.loadWithRowFabric(file, PoolRow)
  }
}
