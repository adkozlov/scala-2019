import java.util

import org.scalatest.Matchers

class TreapTest extends org.scalatest.FlatSpec with Matchers {

    it should "initialize treap correctly" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 1))
        expected.add((1, 3))
        expected.add((2, 1))
        expected.add((3, 4))
        expected.add((100, 2))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        treap.foreach(x => found.add((x, treap.get(x))))

        found should be (expected)
    }

    it should "add elements to the multiset correctly" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)

        treap.add(0)
        treap.add(1)
        treap.add(2)
        treap.add(3)
        treap.add(4)
        treap.add(4)
        treap.add(5)
        treap.add(-10)

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 2))
        expected.add((0, 1))
        expected.add((1, 4))
        expected.add((2, 2))
        expected.add((3, 5))
        expected.add((4, 2))
        expected.add((5, 1))
        expected.add((100, 2))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        treap.foreach(x => found.add((x, treap.get(x))))

        found should be (expected)
    }

    it should "remove elements from the multiset correctly" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)

        treap.add(0)
        treap.add(1)
        treap.add(2)
        treap.add(3)
        treap.add(4)
        treap.add(4)
        treap.add(5)
        treap.add(-10)

        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)

        treap.remove(100)
        treap.remove(1)
        treap.remove(1)
        treap.remove(2)
        treap.remove(2)

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 2))
        expected.add((0, 1))
        expected.add((1, 2))
        expected.add((3, 5))
        expected.add((4, 2))
        expected.add((100, 1))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        treap.foreach(x => found.add((x, treap.get(x))))

        found should be (expected)
    }

    it should "put values to elements in the multiset correctly" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)

        treap.add(0)
        treap.add(1)
        treap.add(2)
        treap.add(3)
        treap.add(4)
        treap.add(4)
        treap.add(5)
        treap.add(-10)

        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)
        treap.remove(5)

        treap.remove(100)
        treap.remove(1)
        treap.remove(1)
        treap.remove(2)
        treap.remove(2)

        treap.put(100, 100)
        treap.put(-50, 50)
        treap.put(-200, -200)
        treap.put(40, 0)
        treap.put(4, treap.get(4) + 20)

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-50, 50))
        expected.add((-10, 2))
        expected.add((0, 1))
        expected.add((1, 2))
        expected.add((3, 5))
        expected.add((4, 22))
        expected.add((100, 100))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        treap.foreach(x => found.add((x, treap.get(x))))

        found should be (expected)
    }

    it should "merge multisets correctly" in {
        val treap1 = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)
        val treap2 = new Treap[BigInt](-1, 0, -1, 0, 0, 1, 1, 5, 5, 100)
        val merged = treap1 | treap2

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 1))
        expected.add((-1, 2))
        expected.add((0, 3))
        expected.add((1, 5))
        expected.add((2, 1))
        expected.add((3, 4))
        expected.add((5, 2))
        expected.add((100, 3))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        merged.foreach(x => found.add((x, merged.get(x))))

        found should be (expected)
    }

    it should "intersect multisets correctly" in {
        val treap1 = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)
        val treap2 = new Treap[BigInt](-1, 0, -1, 0, 0, 1, 1, 5, 5, 100, 3)
        val merged = treap1 & treap2

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((1, 5))
        expected.add((3, 5))
        expected.add((100, 3))

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        merged.foreach(x => found.add((x, merged.get(x))))

        found should be (expected)
    }

    it should "correctly work with simple for comprehensions" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)

        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        for (x <- treap) {
            found.add((x, treap.get(x)))
        }

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 1))
        expected.add((1, 3))
        expected.add((2, 1))
        expected.add((3, 4))
        expected.add((100, 2))

        found should be (expected)
    }

    it should "correctly work with for comprehensions with conditions" in {
        val treap = new Treap[BigInt](1, 2, 1, 1, 3, 3, 3, 3, -10, 100, 100)
        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]

        for (
            x <- treap
            if (x - treap.get(x)) % 2 == 0
        ) {
            found.add((x, treap.get(x)))
        }

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((1, 3))
        expected.add((100, 2))

        found should be (expected)
    }

    it should "correctly work with filter and map" in {
        val treap = new Treap[BigInt](1, -10, -10, -10, 3, 50, -10, 50, 50, 50, -10, 100, 100, 100, 2, 0, -10)
        val found: util.ArrayList[Int] = new util.ArrayList[Int]

        treap.filter(x => x % 2 == 0)
            .map(x => BigInt(treap.get(x)))
            .filter(x => x > 2)
            .foreach(x => found.add(x.toInt))

        val expected: util.ArrayList[Int] = new util.ArrayList[Int]
        expected.add(3)
        expected.add(4)
        expected.add(6)

        found should be (expected)
    }

    it should "correctly work with withFilter" in {
        val treap = new Treap[BigInt](1, -10, -10, -10, 3, 50, -10, 50, 50, 50, -10, 100, 100, 100, 2, 0, -10)
        val found: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]

        treap.withFilter(x => x % 2 == 0)
        treap.foreach(x => found.add((x, treap.get(x))))

        val expected: util.ArrayList[(BigInt, Int)] = new util.ArrayList[(BigInt, Int)]
        expected.add((-10, 6))
        expected.add((0, 1))
        expected.add((2, 1))
        expected.add((50, 4))
        expected.add((100, 3))

        found should be (expected)
    }
}
