package ru.spbau.jvm.scala.hw2

import scala.reflect.ClassTag
import scala.util.Random

case class Tree[A](x: A,
                   y: Int,
                   var cnt: Int = 1,
                   left: Tree[A] = null,
                   right: Tree[A] = null)(implicit ev$1: A => Ordered[A]) {}

object Tree {
  def build[A](elements: Array[A])(implicit ev$1: A => Ordered[A]): Tree[A] = {
    var tree: Tree[A] = null
    for (element <- elements) {
      tree = add(tree, element)
    }
    tree
  }

  def add[A](tree: Tree[A], x: A, cnt: Int = 1)(implicit ev$1: A => Ordered[A]): Tree[A] = {
    if (tree == null)
      new Tree(x, Random.nextInt(), cnt)
    else if (tree.x == x) {
      tree.cnt += cnt
      tree
    } else if (tree.x < x) {
      new Tree(tree.x, tree.y, tree.cnt, tree.left, add(tree.right, x, cnt))
    } else {
      new Tree(tree.x, tree.y, tree.cnt, add(tree.left, x, cnt), tree.right)
    }
  }

  def getKeys[A: ClassTag](tree: Tree[A])(implicit ev$1: A => Ordered[A]): Array[A] =
    if (tree == null)
      Array.empty[A]
    else
      getKeys(tree.left) ++ Array(tree.x) ++ getKeys(tree.right)

  def toPairsArray[A](tree: Tree[A])(implicit ev$1: A => Ordered[A]): Array[(A, Int)] =
    if (tree == null)
      Array.empty[(A, Int)]
    else {
      val left = toPairsArray(tree.left)
      val right = toPairsArray(tree.right)
      left ++ Array((tree.x, tree.cnt)) ++ right
    }

  @scala.annotation.tailrec
  def cnt[A](tree: Tree[A], x: A)(implicit ev$1: A => Ordered[A]): Int =
    if (tree == null)
      0
    else if (tree.x == x)
      tree.cnt
    else if (tree.x < x)
      cnt(tree.right, x)
    else
      cnt(tree.left, x)

  def remove[A](tree: Tree[A], x: A, cnt: Int = 1)(implicit ev$1: A => Ordered[A]): Tree[A] =
    if (tree == null)
      null
    else if (tree.x == x) {
      if (tree.cnt <= cnt)
        merge(tree.left, tree.right)
      else
        new Tree(tree.x, tree.y, tree.cnt - cnt, tree.left, tree.right)
    } else if (tree.x < x)
      new Tree(tree.x, tree.y, tree.cnt, tree.left, remove(tree.right, x, cnt))
    else
      new Tree(tree.x, tree.y, tree.cnt, remove(tree.left, x, cnt), tree.right)

  def merge[A](first: Tree[A], second: Tree[A])(implicit ev$1: A => Ordered[A]): Tree[A] = {
    if (first == null)
      return second
    if (second == null)
      return first
    if (first.y > second.y)
      new Tree(first.x, first.y, first.cnt, first.left, merge(first.right, second))
    else
      new Tree(second.x, second.y, second.cnt, merge(first, second.left), second.right)
  }

  def split[A](tree: Tree[A], x: A)(implicit ev$1: A => Ordered[A]): (Tree[A], Tree[A]) =
    if (tree.x <= x) {
      if (tree.right == null) {
        (tree, null)
      } else {
        val (l: Tree[A], r: Tree[A]) = split(tree.right, x)
        (new Tree(tree.x, tree.y, tree.cnt, tree.left, l), r)
      }
    } else {
      if (tree.left == null) {
        (null, tree)
      } else {
        val (l: Tree[A], r: Tree[A]) = split(tree.left, x)
        (l, new Tree(tree.x, tree.y, tree.cnt, r, tree.right))
      }
    }

  def map[A, B](tree: Tree[A], function: A => B)(implicit ev$1: B => Ordered[B]): Tree[B] = {
    if (tree == null)
      null
    else
      new Tree(function(tree.x), tree.y, tree.cnt, map(tree.left, function), map(tree.right, function))
  }

  def filter[A](tree: Tree[A], function: A => Boolean)(implicit ev$1: A => Ordered[A]): Tree[A] = {
    if (tree == null)
      null
    else if (function(tree.x))
      new Tree(tree.x, tree.y, tree.cnt, filter(tree.left, function), filter(tree.right, function))
    else
      merge(filter(tree.left, function), filter(tree.right, function))
  }
}

