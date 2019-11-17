package ru.spbau.jvm.scala

import java.io.{BufferedReader, File, InputStreamReader, OutputStreamWriter}
import java.nio.file.FileSystems
import java.sql.DriverManager
import java.util.stream.Collectors

import org.squeryl.PrimitiveTypeMode
import org.squeryl.adapters.SQLiteAdapter
import org.squeryl.{KeyedEntity, Schema, Session, SessionFactory}


//case class Bar(name: Option[String]) extends KeyedEntity[Long] {
//  val id: Long = 0
//}

object PrimitiveType extends PrimitiveTypeMode

import ru.spbau.jvm.scala.PrimitiveType._

//object AppDB extends Schema {
//  val barTable = table[Bar]("bar")
//}

class User(
            val id: Int,
            val name: String,
            val surname: String,
            val number_id: Int
          ) extends KeyedEntity[Int]

class PhoneNumber(
                   val id: Int,
                   val number: String
                 ) extends KeyedEntity[Int]

object BillingSystem extends Schema {
  val user = table[User]("User")
  val number = table[PhoneNumber]("Number")
}

object Main1 {
  val sqliteInit: String =
    s"""
      |.mode csv
      |create table User (
      |    id INTEGER not null primary key autoincrement,
      |    name varchar(128) not null,
      |    surname varchar(128) not null,
      |    number_id INTEGER not null
      |  );
      |create table Number (
      |    id INTEGER not null primary key,
      |    number varchar(128) not null
      |  );
      |.import ${FileSystems.getDefault.getPath("resources").resolve("User.txt")} User
      |.import ${FileSystems.getDefault.getPath("resources").resolve("Number.txt")} Number
    """.stripMargin

  def initDatabase(databaseFilePath: String): Unit = {
    val process = Runtime.getRuntime.exec(s"sqlite3 $databaseFilePath")
    val writer = new OutputStreamWriter(process.getOutputStream)
    writer.write(sqliteInit)
    writer.close()
    process.waitFor()
  }

  def connect(databaseFilePath: String) = {
    val url = s"jdbc:sqlite:file:$databaseFilePath"
    DriverManager.getConnection(url)
  }

  def main(args: Array[String]): Unit = {
    val tempFile = File.createTempFile("phonebook", ".db").getAbsoluteFile
    tempFile.deleteOnExit()
    println(s"temp file name:${tempFile.getPath}")


    val conn = connect(tempFile.getPath)
    initDatabase(tempFile.getPath)

    SessionFactory.concreteFactory = Some(()=>
      Session.create(
        conn,
        new SQLiteAdapter)
    )


    inTransaction {
      import BillingSystem._
      printDdl

      val q = from(user)(s =>
        where(s.id gt 0)
          select s
      )

      for (x <- q) {
        println(x.id, x.name, x.surname)
      }

      println("QEQ")
    }

    val reader = new BufferedReader(new InputStreamReader(System.in))
    var query = "q"
    while (query != "q") {
      query = reader.readLine()
      try {
        val res = conn.prepareStatement(query).executeQuery()
        val rsmd = res.getMetaData
        while (res.next) {
          var i = 0
          for (i <- 1 to rsmd.getColumnCount) {
            if (i > 1) System.out.print(",  ")
            val columnValue = res.getString(i)
            System.out.print(s"$columnValue :${rsmd.getColumnName(i)}")
          }
          System.out.println("")
        }
      } catch {
        case e: Exception => System.out.println(e)
      }

    }
//
//    conn.close()
  }
}

