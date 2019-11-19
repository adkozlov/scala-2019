import java.time.LocalDate
import java.util
import java.util.stream.Collectors

import scala.io.Source

class DataBase {

    private val DATE_PATTERN = "(\\d{2}).(\\d{2}).(\\d{4})"
    private val CALL_RE = s"(\\d+),(\\d+),([0-9\\.]+),([0-9\\.]+),$DATE_PATTERN".r
    private val MATCHING_RE = "([a-zA-Z.\\ ]+),([a-zA-Z.\\ ]+),(\\+?\\d{11}),(\\d+)".r

    private val calls = new util.ArrayList[Call]()
    private val matching = new util.HashMap[Int, User]()

    def loadDataBase(callsFile: String, matchingFile: String): Unit = {
        loadCalls(callsFile)
        loadMatchings(matchingFile)
    }

    private def loadCalls(callsFile: String): Unit = {
        calls.clear()
        for (record <- Source.fromFile(callsFile).getLines()) {
            val call = record match {
                case CALL_RE(idFrom, idTo, duration, cost, day, month, year) =>
                    new Call(
                        idFrom.toInt,
                        idTo.toInt,
                        duration.toDouble,
                        cost.toDouble,
                        LocalDate.of(year.toInt, month.toInt, day.toInt)
                    )
                case _ =>
                    throw new IllegalArgumentException(f"Unrecognised format of record: '$record'")
            }
            calls.add(call)
        }
    }

    private def loadMatchings(matchingsFile: String): Unit = {
        matching.clear()
        for (record <- Source.fromFile(matchingsFile).getLines()) {
            record match {
                case MATCHING_RE(name, surname, number, id) =>
                    matching.put(id.toInt, new User(name, surname, number, id.toInt))
                case _ =>
                    throw new IllegalArgumentException(f"Unrecognised format of record: '$record'")
            }
        }
    }

    def getCallsInPeriod(from: LocalDate, to: LocalDate): util.List[Call] =
        calls.stream().filter(x => x.isBetween(from, to)).collect(Collectors.toList[Call])

    def getUserById(id: Int): User = matching.get(id)

    def getUserByNameSurname(name: String, surname: String): User =
        matching.entrySet()
            .stream()
            .filter(entry => entry.getValue.name.equals(name) && entry.getValue.surname.equals(surname))
            .findAny().get().getValue
}
