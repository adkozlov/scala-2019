package ru.spbau.jvm.scala

import scala.util.Random

class Multiset[T](values: T*)(implicit ord: Ordering[T]) {
    private var root: Node = _

    private val random: Random = new Random()

    values.foreach(value => this.change(value, 1))

    private def split(v: Node, value: T, strict: Boolean): (Node, Node) = {
        if (v == null) {
            return (null, null)
        }

        if (ord.lt(v.value, value) || (!strict && ord.lteq(v.value, value))) {
            val tmp = split(v.right, value, strict)
            v.right = tmp._1
            (v, tmp._2)
        } else {
            val tmp = split(v.left, value, strict)
            v.left = tmp._2
            (tmp._1, v)
        }
    }

    private def merge(a: Node, b: Node): Node = {
        if (a == null)
            return b
        if (b == null)
            return a

        if (a.y < b.y) {
            a.right = merge(a.right, b)
            a
        } else {
            b.left = merge(a, b.left)
            b
        }
    }

    private def splitToThree(value: T): (Node, Node, Node) = {
        val tmp1 = split(root, value, strict = true)
        val tmp2 = split(tmp1._2, value, strict = false)
        (tmp1._1, tmp2._1, tmp2._2)
    }

    def change(value: T, delta: Int): Int = {
        val tmp = splitToThree(value)

        if (delta == 0) {
            root = merge(tmp._1, merge(tmp._2, tmp._3))
            return if (tmp._2 == null) 0 else tmp._2.cnt
        }

        if (delta > 0) {
            var mid: Node = null
            if (tmp._2 == null) {
                mid = Node(value, delta)
            } else {
                mid = tmp._2
                mid.cnt += delta
            }
            root = merge(tmp._1, merge(mid, tmp._3))
            mid.cnt
        } else {
            var mid: Node = null
            if (tmp._2 != null) {
                mid = tmp._2
                mid.cnt += delta
                if (mid.cnt < 0) {
                    mid = null
                }
            }
            root = merge(tmp._1, merge(mid, tmp._3))
            if (mid == null) 0 else mid.cnt
        }
    }

    def apply(value: T): Int = change(value, 0)

    def foreach(f: T => Unit): Unit = {
        foreach(root, f)
    }

    def foreach(v: Node, f: T => Unit): Unit = {
        if (v == null) {
            return
        }
        foreach(v.left, f)
        f(v.value)
        foreach(v.right, f)
    }

    def toList(): List[(T, Int)] = {
        val builder = List.newBuilder[(T, Int)]
        for (value <- this) {
            builder.addOne((value, this(value)))
        }
        builder.result()
    }

    def filter(p: T => Boolean): Multiset[T] = {
        val result = new Multiset[T]
        for (value <- this) {
            if (p(value)) {
                result.change(value, this(value))
            }
        }
        result
    }

    def |(other: Multiset[T]): Multiset[T] = {
        val result = new Multiset[T]
        for (value <- this) {
            result.change(value, this(value))
        }
        for (value <- other) {
            result.change(value, other(value))
        }
        result
    }

    def &(other: Multiset[T]): Multiset[T] = {
        val result = new Multiset[T]
        for (value <- this) {
            val inOther = other(value)
            result.change(value, math.min(inOther, this(value)))
        }
        result
    }

    def map[S](f: T => S)(implicit _ord: Ordering[S]): Multiset[S] = {
        val result = new Multiset[S]
        for (value <- this) {
            result.change(f(value), this(value))
        }
        result
    }

    private case class Node(value: T,
                            var cnt: Int,
                            var left: Node = null,
                            var right: Node = null,
                            y: Int = random.nextInt()
                           )
}
