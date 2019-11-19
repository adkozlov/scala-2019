import java.util

import scala.io.Source

object DataBase {

    private val CALL_RE = "([a-zA-Z.\\ ]+),([a-zA-Z.\\ ]+),(\\+?\\d{11}),([0-9\\.]+),([0-9\\.]+)".r
    private val MATCHING_RE = "([0-9a-f]+),(\\+?\\d{11})".r

    private val calls = new util.ArrayList[Call]()
    private val matchings = new util.ArrayList[Matching]()

    def loadDataBase(callsFile: String, matchingsFile: String): Unit = {
        loadCalls(callsFile)
        loadMatchings(matchingsFile)
    }

    private def loadCalls(callsFile: String): Unit = {
        println(f"load calls from $callsFile")
        for (record <- Source.fromFile(callsFile).getLines()) {
            val call = record match {
                case CALL_RE(name, surname, callee, duration, cost) =>
                    new Call(name, surname, callee, duration.toDouble, cost.toDouble)
                case _ => throw new IllegalArgumentException(f"Unrecognised format of record: '$record'")
            }
            calls.add(call)
        }
        calls.forEach(println)
    }

    private def loadMatchings(matchingsFile: String): Unit = {
        println("load matchings")
        for (record <- Source.fromFile(matchingsFile).getLines()) {
            val matching = record match {
                case MATCHING_RE(hash, number) => new Matching(hash, number)
                case _ => throw new IllegalArgumentException(f"Unrecognised format of record: '$record'")
            }
            matchings.add(matching)
        }
        matchings.forEach(println)
    }

    private class Call(
                  val name: String,
                  val surname: String,
                  val callee: String,
                  val duration: Double,
                  val cost: Double
              ) {
        def getName: String = name
        def getSurname: String = surname
        override def toString: String =
            s"$name | $surname | $callee | $duration | $cost"
    }

    private class Matching(val ownerHash: String, phoneNumber: String) {
        override def toString: String = phoneNumber
    }

}
