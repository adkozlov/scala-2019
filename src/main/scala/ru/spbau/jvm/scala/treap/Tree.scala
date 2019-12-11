package ru.spbau.jvm.scala.treap

sealed abstract class Tree[K, P]
case class Leaf[K, P]() extends Tree[K, P]
case class Node[K, P](key: K,
                      pri: P,
                      left: Tree[K, P] = Leaf(),
                      right: Tree[K, P] = Leaf())
    extends Tree[K, P]
