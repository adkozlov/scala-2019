package ru.spbau.jvm.scala

//import slick.model.Table
import java.sql.DriverManager

import slick.jdbc.H2Profile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Tables {
  class User(tag: Tag) extends Table[(Int, String, String, Int)](tag, "User") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def surname = column[String]("surname")
    def number_id = column[Int]("number_id")

    override def * = (id, name, surname, number_id)
  }

  class PhoneNumber(tag: Tag) extends Table[(Int, String)](tag, "Number") {
    def id = column[Int]("id")
    def number = column[String]("number")
    override def * = (id, number)
  }

  lazy val users = TableQuery[User]
  lazy val numbers = TableQuery[PhoneNumber]
}

import Tables._

object Main1 {

  def main(args: Array[String]): Unit = {
    //    val foo: Foo = Bar()
    //    println(foo.foo)
//    Class.forName("org.relique.jdbc.csv.CsvDriver")
    val url = "jdbc:sqlite:memory:?cache=shared"
//    val url = "jdbc:relique:csv:resources?" +
//      "separator=," + "&" + "fileExtension=.txt"
    val conn = DriverManager.getConnection(url)

    val db = Database.forURL(url)


//    val query = users.map(p => (p.id,p.name,p.number_id))
//    db.run(query.result).foreach(s => s.foreach(p => println(p)))

//    val s = Await.result(db.run(query.result), Duration.Inf)
//    s.foreach(p => println(p))
val query = users.map(p => (p.id,p.name,p.number_id))

    val tRes = Await.result(db.run(query.result), Duration.Inf)

//    val sql =
//    //#sqlQueryProjection*
//      sql"select * from PERSON".as[User]
//    //#sqlQueryProjection*
//    val slick =
//      users.result
//
//    val (sqlRes, slickRes) = Await.result(db.run(sql zip slick), Duration.Inf)

//    users.map(p => println(p.id, p.name, p.surname, p.number_id)).result
//     //    conn.close()
//
  }
}

