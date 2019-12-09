package ru.spbau.jvm.scala.hw2

import scala.reflect.ClassTag

class Multiset[A <% Ordered[A] : ClassTag]() {

  private def this(tree: Tree[A]) {
    this()
    this.tree = tree
  }

  def this(elements: A*) {
    this()
    tree = Tree.build(elements.toArray)
  }

  var tree: Tree[A] = _

  def remove(x: A): Unit = {
    tree = Tree.remove(tree, x)
  }

  def add(x: A): Unit = {
    tree = Tree.add(tree, x)
  }

  def apply(x: A): Int = Tree.cnt(tree, x)

  def |(other: Multiset[A]): Multiset[A] = {
    var newTree = tree
    for ((x, cnt) <- Tree.toPairsArray(other.tree)) {
      newTree = Tree.add(newTree, x, cnt)
    }
    new Multiset[A](newTree)
  }

  def &(other: Multiset[A]): Multiset[A] = {
    var newTree: Tree[A] = null
    for (x <- Tree.getKeys(other.tree)) {
      val min = Math.min(Tree.cnt(tree, x), Tree.cnt(other.tree, x))
      if (min > 0) {
        newTree = Tree.add(newTree, x, min)
      }
    }
    new Multiset(newTree)
  }

  def foreach(function: A => Any): Unit = {
    Tree.toPairsArray(tree).flatMap(x => List.fill(x._2)(x._1)).foreach(function)
  }

  def map[B: ClassTag](function: A => B)(implicit ev$1: B => Ordered[B]): Multiset[B] = new Multiset[B](Tree.map(tree, function))

  def filter(function: A => Boolean): Multiset[A] = new Multiset(Tree.filter(tree, function))

  override def toString: String = Tree.toPairsArray(tree).map(x => x._1 + " -> " + x._2).mkString("[", ", ", "]")
}

