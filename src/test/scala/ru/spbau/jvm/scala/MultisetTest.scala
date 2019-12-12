package ru.spbau.jvm.scala

import org.scalatest.matchers.should.Matchers

class MultisetTest extends org.scalatest.FlatSpec with Matchers {
    it should "add values" in {
        val multiset = new Multiset[Int]
        multiset.change(0, 2)
        assert(multiset(0) == 2)
    }

    it should "rm values" in {
        val multiset = new Multiset[Int](0, 0, 0, 1)
        multiset.change(0, -1)
        assert(multiset(0) == 2)
        multiset.change(0, -100)
        assert(multiset(0) == 0)
    }

    it should "unite" in {
        val m1 = new Multiset[Int](0, 0, 1, 3)
        val m2 = new Multiset[Int](0, 2)
        assert((m1 | m2).toList() == List(
            (0, 3),
            (1, 1),
            (2, 1),
            (3, 1)
        ))
    }

    it should "intersect" in {
        val m1 = new Multiset[Int](0, 0, 1, 3)
        val m2 = new Multiset[Int](0, 2)
        assert((m1 & m2).toList() == List(
            (0, 1)
        ))
    }

    it should "map values" in {
        val multiset = new Multiset[Int](10, 1, 5)
        assert(multiset.map(x => x.toString).toList() == List(
            ("1", 1),
            ("10", 1),
            ("5", 1)
        ))
    }

    it should "filter values" in {
        val multiset = new Multiset[Int](1, 2, 3, 4, 5, 6, 7, 8)
        assert(multiset.filter(x => (x & (x - 1)) == 0).toList() == List(
            (1, 1),
            (2, 1),
            (4, 1),
            (8, 1)
        ))
    }

}