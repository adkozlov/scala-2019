package ru.spbau.jvm.scala

import org.scalatest.{FlatSpec, Matchers}
import ru.spbau.jvm.scala.command.{Avg, BiggestTalker, CallsTotalFrom, CostTotalFrom, Help, Max, MostCalls, Number}

class DBSpec extends FlatSpec with Matchers {
  "calls" should "give correct answer" in {
    CallsTotalFrom.execute(Nil) should be (Right(
      """|Operations.Callee | Operations.Duration | Clients.FirstName | Clients.SecondName | Cost
         |+7 953 5552 981 | 1.0 | Juan | Galloway | 228.0
         |+7 915 5568 089 | 2.0 | Juan | Galloway | 456.0
         |+7 915 5577 497 | 3.0 | Juan | Galloway | 684.0
         |+7 915 5568 089 | 4.0 | Landen | Mcdaniel | 912.0
         |+7 953 5552 981 | 5.0 | Alexis | Dudley | 1140.0
         |+7 915 5580 668 | 6.0 | Trevin | Mckay | 1368.0
         |+7 953 5558 031 | 7.0 | Diego | Rose | 1596.0
         |+7 915 5577 497 | 8.0 | Daniela | Huffman | 1824.0
         |+7 904 5553 133 | 9.0 | Moriah | Nguyen | 2052.0
         |+7 935 5511 983 | 10.0 | John | Doe | 2280.0
         |+7 915 5531 089 | 11.0 | John | Doe | 2508.0""".stripMargin))

    CallsTotalFrom.execute(List("20.11.2019")) should be (Right(
      """|Operations.Callee | Operations.Duration | Clients.FirstName | Clients.SecondName | Cost
         |+7 915 5577 497 | 3.0 | Juan | Galloway | 684.0
         |+7 915 5568 089 | 4.0 | Landen | Mcdaniel | 912.0
         |+7 953 5552 981 | 5.0 | Alexis | Dudley | 1140.0
         |+7 915 5580 668 | 6.0 | Trevin | Mckay | 1368.0
         |+7 953 5558 031 | 7.0 | Diego | Rose | 1596.0
         |+7 915 5577 497 | 8.0 | Daniela | Huffman | 1824.0
         |+7 904 5553 133 | 9.0 | Moriah | Nguyen | 2052.0
         |+7 935 5511 983 | 10.0 | John | Doe | 2280.0
         |+7 915 5531 089 | 11.0 | John | Doe | 2508.0""".stripMargin))

    CallsTotalFrom.execute(List("21.11.2019", "23.11.2019")) should be (Right(
      "Operations.Callee | Operations.Duration | Clients.FirstName | Clients.SecondName | Cost"))
  }

  "total" should "give correct answer" in {
    CostTotalFrom.execute(Nil) should be (Right("15048.0"))
    CostTotalFrom.execute(List("20.11.2019")) should be (Right("14364.0"))
    CostTotalFrom.execute(List("21.11.2019", "23.11.2019")) should be (Right("0.0"))
  }

  "avg" should "give correct answer" in {
    Avg.execute(Nil) should be (Right("6.0s"))
  }

  "number" should "give correct answer" in {
    Number.execute(List("Juan", "Galloway")) should be (Right("+7 927 5559 586"))
    Number.execute(List("John", "Doe")) should be (Right("+7 907 5559 586"))
    Number.execute(List("Kailyn", "Lewis")) should be (Right("+7 935 5311 983"))
  }

  "max" should "give correct answer" in {
    Max.execute(Nil) should be (Right("11.0s"))
  }

  "biggest_talker" should "give correct answer" in {
    BiggestTalker.execute(Nil) should be (Right("John Doe"))
  }

  "most_calls" should "give correct answer" in {
    MostCalls.execute(Nil) should be (Right("Juan Galloway"))
  }

  "help" should "give correct answer" in {
    Help.execute(Nil) should be (Right(
      """|calls from DATETIME to DATETIME — display all calls made in the given period of time
         |avg — average length of call
         |total from DATETIME to DATETIME — total cost of calls in the given period of time
         |number VARCHAR VARCHAR — gets phone number of given client
         |biggest_talker — client with most call time
         |max — max call duration
         |most_calls — client with most amount of calls
         |help — list of available commands""".stripMargin))
  }
}
