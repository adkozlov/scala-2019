package hw03

object Main {

    import Zippable._

    def main(args: Array[String]): Unit = {
        val left = "hello" :: 42 :: false :: HNil
        val right = true :: "world" :: 53 :: HNil
        val result = left zip right

        println(result)

        val (lpart, rpart) = result.split(Positive(Positive(Positive(Zero))))

        println()
        println(lpart)
        println(rpart)
    }
}
