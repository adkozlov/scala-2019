package ru.spbau.jvm.scala.db

import java.nio.file.{Path, Paths}

import ru.spbau.jvm.scala.db.tables.{CallsTable, PhonesTable, Table, UsersTable}

class BillingDB {
  val users = new UsersTable()
  val phones = new PhonesTable()
  val calls = new CallsTable()
  private[this] val usersPath = "users.txt"
  private[this] val phonesPath = "phones.txt"
  private[this] val callsPath = "calls.txt"

  def loadTables(pathFolder: String): Unit = {
    val path: Path = Paths.get(pathFolder)
    users.load(path.resolve(usersPath).toString)
    phones.load(path.resolve(phonesPath).toString)
    calls.load(path.resolve(callsPath).toString)
  }
}
