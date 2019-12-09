package ru.spbau.jvm.scala.hw2

import scala.util.Random

case class Tree[A](x: A,
                   y: Int,
                   left: Option[Tree[A]] = None,
                   right: Option[Tree[A]] = None)(implicit ev$1: A => Ordered[A]) {}

object Tree {
  def build[A](elements: Array[A])(implicit ev$1: A => Ordered[A]): Option[Tree[A]] = {
    var tree: Option[Tree[A]] = None
    for (element <- elements) {
      tree = add(tree, element)
    }
    tree
  }

  def add[A](tree: Option[Tree[A]], x: A, y: Int = Random.nextInt())(implicit ev$1: A => Ordered[A]): Option[Tree[A]] = {
    if (tree.isEmpty)
      Option(new Tree(x, y))
    else if (tree.get.y > y) {
      val (l: Option[Tree[A]], r: Option[Tree[A]]) = split(tree, x)
      Option(new Tree(x, y, l, r))
    } else if (tree.get.x >= x)
      Option(new Tree(tree.get.x, tree.get.y, Tree.add(tree.get.left, x, y), tree.get.right))
    else
      Option(new Tree(tree.get.x, tree.get.y, tree.get.left, Tree.add(tree.get.right, x, y)))

  }


  def count[A](tree: Option[Tree[A]], x: A)(implicit ev$1: A => Ordered[A]): Int =
    if (tree.isEmpty)
      0
    else if (tree.get.x == x)
      1 + count(tree.get.left, x)
    else if (tree.get.x < x)
      count(tree.get.right, x)
    else
      count(tree.get.left, x)

  def remove[A](tree: Option[Tree[A]], x: A, cnt: Int = 1)(implicit ev$1: A => Ordered[A]): Option[Tree[A]] =
    if (tree.isEmpty)
      None
    else if (tree.get.x == x) {
      Tree.merge(tree.get.left, tree.get.right)
    } else if (tree.get.x < x)
      Option(new Tree(tree.get.x, tree.get.y, tree.get.left, remove(tree.get.right, x, cnt)))
    else
      Option(new Tree(tree.get.x, tree.get.y, remove(tree.get.left, x, cnt), tree.get.right))


  def merge[A](first: Option[Tree[A]], second: Option[Tree[A]])(implicit ev$1: A => Ordered[A]): Option[Tree[A]] = {
    if (first.isEmpty)
      return second
    if (second.isEmpty)
      return first
    if (first.get.y > second.get.y)
      Option(new Tree(first.get.x, first.get.y, first.get.left, merge(first.get.right, second)))
    else
      Option(new Tree(second.get.x, second.get.y, merge(first, second.get.left), second.get.right))
  }

  def split[A](tree: Option[Tree[A]], x: A)(implicit ev$1: A => Ordered[A]): (Option[Tree[A]], Option[Tree[A]]) =
    if (tree.isEmpty) {
      (None, None)
    }
    else if (tree.get.x <= x) {
      if (tree.get.right.isEmpty) {
        (tree, None)
      } else {
        val (l: Option[Tree[A]], r: Option[Tree[A]]) = split(tree.get.right, x)
        (Option(new Tree(tree.get.x, tree.get.y, tree.get.left, l)), r)
      }
    } else {
      if (tree.get.left.isEmpty) {
        (None, tree)
      } else {
        val (l: Option[Tree[A]], r: Option[Tree[A]]) = split(tree.get.left, x)
        (l, Option(new Tree(tree.get.x, tree.get.y, r, tree.get.right)))
      }
    }

  def map[A, B](tree: Option[Tree[A]], function: A => B)(implicit ev$1: B => Ordered[B]): Option[Tree[B]] = {
    if (tree.isEmpty)
      None
    else {
      val left = map(tree.get.left, function)
      Option(new Tree(function(tree.get.x), tree.get.y, left, map(tree.get.right, function)))
    }
  }

  def forEach[A, B](tree: Option[Tree[A]], function: A => Any): Unit = {
    if (tree.isEmpty)
      None
    else {
      forEach(tree.get.left, function)
      function(tree.get.x)
      forEach(tree.get.right, function)
    }
  }

  def filter[A](tree: Option[Tree[A]], function: A => Boolean)(implicit ev$1: A => Ordered[A]): Option[Tree[A]] = {
    if (tree.isEmpty)
      None
    else {
      val left = filter(tree.get.left, function)
      if (function(tree.get.x))
        Option(new Tree(tree.get.x, tree.get.y, left, filter(tree.get.right, function)))
      else
        merge(left, filter(tree.get.right, function))
    }
  }
}

