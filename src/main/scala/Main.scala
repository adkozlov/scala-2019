

class A {
    var x = 10
}

object Main {

    def main(args: Array[String]): Unit = {
        val t1 = new Treap[BigInt](1, 2, 2, 2, 2, 3, 6, 6, 101)
        val t2 = new Treap[BigInt](1, 2, 4, 2, 2, 5)
        val i = t1 & t2
        val u = t1 | t2

        u.foreach(key => println(s"$key ${u.get(key)}"))
        println("\n")
//
//        for (
//            x <- u
//            if x % 2 == 0
//        ) {
//            println(x)
//        }
    }
}
