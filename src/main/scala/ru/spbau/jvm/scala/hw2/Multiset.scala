package ru.spbau.jvm.scala.hw2

import scala.reflect.ClassTag

class Multiset[A <% Ordered[A] : ClassTag]() {

  private def this(tree: Option[Tree[A]]) {
    this()
    this.tree = tree
  }

  def this(elements: A*) {
    this()
    tree = Tree.build(elements.toArray)
  }

  var tree: Option[Tree[A]] = None

  def remove(x: A): Unit = {
    tree = Tree.remove(tree, x)
  }

  def add(x: A): Unit = {
    tree = Tree.add(tree, x)
  }

  def apply(x: A): Int = Tree.count(tree, x)

  def &(other: Multiset[A]): Multiset[A] = {
    var newTree: Option[Tree[A]] = None
    Tree.forEach(other.tree, (x: A) => {
      if (Tree.count(newTree, x) == 0) {
        val its = math.min(Tree.count(tree, x), Tree.count(other.tree, x))
        for (_ <- 1 to its) {
          newTree = Tree.add(newTree, x)
        }
      }
    })
    new Multiset(newTree)
  }

  def contains(x: A): Boolean = Tree.count(tree, x) > 0

  def |(other: Multiset[A]): Multiset[A] = {
    var newTree: Option[Tree[A]] = None
    Tree.forEach(tree, (x: A) => newTree = Tree.add(newTree, x))
    Tree.forEach(other.tree, (x: A) => newTree = Tree.add(newTree, x))
    new Multiset(newTree)
  }

  def foreach(function: A => Any): Unit = Tree.forEach(tree, function)

  def map[B: ClassTag](function: A => B)(implicit ev$1: B => Ordered[B]): Multiset[B]
  = new Multiset[B](Tree.map(tree, function))

  def filter(function: A => Boolean): Multiset[A] = new Multiset(Tree.filter(tree, function))

  override def toString: String = {
    var resultString = ""
    resultString += "["
    var newTree: Option[Tree[A]] = None
    Tree.forEach(tree, (x: A) => {
      if (Tree.count(newTree, x) == 0) {
        if (newTree.isDefined) {
          resultString += ", "
        }
        newTree = Tree.add(newTree, x)
        resultString += x + " -> " + Tree.count(tree, x)
      }
    })
    resultString += "]"
    resultString
  }
}

