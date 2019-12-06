

class A {
    var x = 10
}

object Main {

    def main(args: Array[String]): Unit = {
        val t = new Treap[BigInt](1, 2, 2, 2, 2, 3)

        for (x <- t) {
            println(x)
        }

        val x = Seq(1, 2, 3)
        x.withFilter()

    }

}
