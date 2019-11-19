import java.time.LocalDate
import java.util
import java.util.Collections
import java.util.stream.Collectors

import org.scalatest.Matchers

class DataBaseTest extends org.scalatest.FlatSpec with Matchers {

    it should "give correct set of calls in given period of time" in {
        val dataBase = new DataBase
        dataBase.loadDataBase("resources/calls.txt", "resources/matching.txt")

        val expected = new util.ArrayList[Call]

        expected.add(new Call(10, 2, 300.0, 3.0, LocalDate.of(2019, 7, 1)))
        expected.add(new Call(10, 6, 24.0, 0.24, LocalDate.of(2019, 8, 19)))
        expected.add(new Call(0, 4, 42.0, 0.42, LocalDate.of(2019, 9, 2)))
        expected.add(new Call(8, 9, 15.0, 0.15, LocalDate.of(2019, 11, 23)))

        val found = dataBase.getCallsInPeriod(
            LocalDate.of(2019, 7, 1),
            LocalDate.of(2019, 11, 23)
        ).stream().sorted((o1: Call, o2: Call) => o1.date.compareTo(o2.date)).collect(Collectors.toList[Call])

        found should be (expected)
    }

    it should "give empty set of call if lower bound is greater than uppper bound" in {
        val dataBase = new DataBase
        dataBase.loadDataBase("resources/calls.txt", "resources/matching.txt")

        val found = dataBase.getCallsInPeriod(
            LocalDate.of(2019, 11, 1),
            LocalDate.of(2019, 7, 23)
        ).stream().sorted((o1: Call, o2: Call) => o1.date.compareTo(o2.date)).collect(Collectors.toList[Call])

        assert(found.isEmpty)
    }

    it should "give correct user by id" in {
        val dataBase = new DataBase
        dataBase.loadDataBase("resources/calls.txt", "resources/matching.txt")

        val expected = new User("Jackie", "Chan", "+54470992025", 6)
        val found = dataBase.getUserById(6)

        found should be (expected)
    }

    it should "give correct user by name and surname" in {
        val dataBase = new DataBase
        dataBase.loadDataBase("resources/calls.txt", "resources/matching.txt")

        val expected = new User("Dwayne", "Johnson", "+69604518453", 10)
        val found = dataBase.getUserByNameSurname("Dwayne", "Johnson")

        found should be (expected)
    }
}
