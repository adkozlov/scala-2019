package ru.spbau.jvm.scala.treap

object Treap {
  def split[K, P](splitKey: K, tree: Tree[K, P])(
    implicit ord: K => Ordered[K]
  ): (Tree[K, P], Tree[K, P]) = {
    tree match {
      case Leaf() => (Leaf(), Leaf())
      case Node(key, pri, left, right) =>
        if (key < splitKey) {
          val (t1, t2) = split(splitKey, right)
          (Node(key, pri, left, t1), t2)
        } else {
          val (t1, t2) = split(splitKey, left)
          (t1, Node(key, pri, t2, right))
        }
    }
  }

  def merge[K, P](tree1: Tree[K, P], tree2: Tree[K, P])(
    implicit ord: P => Ordered[P]
  ): Tree[K, P] = {
    (tree1, tree2) match {
      case (Leaf(), _) => tree2
      case (_, Leaf()) => tree1
      case (Node(key1, pri1, left1, right1), Node(key2, pri2, left2, right2)) =>
        if (pri1 < pri2) {
          val t = merge(tree1, left2)
          Node(key2, pri2, t, right2)
        } else {
          val t = merge(right1, tree2)
          Node(key1, pri1, left1, t)
        }
    }
  }

  def insert[K, P](k: K, p: P, tree: Tree[K, P])(
    implicit ordK: K => Ordered[K],
    ordP: P => Ordered[P]
  ): Tree[K, P] = {
    val (t1, t2) = Treap.split(k, tree)
    Treap.merge(Treap.merge(t1, Node(k, p, Leaf(), Leaf())), t2)
  }

  def remove[K, P](removeKey: K, tree: Tree[K, P])(
    implicit ordK: K => Ordered[K],
    ordP: P => Ordered[P]
  ): (Option[P], Tree[K, P]) = {
    tree match {
      case Leaf() => (None, Leaf())
      case Node(key, pri, left, right) =>
        if (key == removeKey) {
          return (Some(pri), Treap.merge(left, right))
        }

        if (key < removeKey) {
          val (opPri, newRight) = remove(removeKey, right)
          (opPri, Node(key, pri, left, newRight))
        } else {
          val (opPri, newLeft) = remove(removeKey, left)
          (opPri, Node(key, pri, newLeft, right))
        }
    }
  }

  def getPriByKey[K, P](
    searchKey: K,
    tree: Tree[K, P]
  )(implicit ordK: K => Ordered[K], ordP: P => Ordered[P]): Option[P] = {
    tree match {
      case Leaf() => None
      case Node(key, pri, left, right) =>
        if (key == searchKey) {
          return Some(pri)
        }
        getPriByKey(searchKey, if (key < searchKey) right else left)
    }
  }

  def toString[K, P](tree: Tree[K, P]): String = {
    tree match {
      case Leaf() => ""
      case Node(key, pri, left, right) =>
        toString(left) ++ " " ++ key.toString + "->" + pri ++ " " ++ toString(
          right
        )
    }
  }

  def foreachKey[R, K, P](f: K => R, tree: Tree[K, P]): Unit = {
    tree match {
      case Leaf() =>
      case Node(key, _, left, right) =>
        foreachKey(f, left)
        f(key)
        foreachKey(f, right)
    }
  }

  def foreach[R, K, P](f: (K, P) => R, tree: Tree[K, P]): Unit = {
    tree match {
      case Leaf() =>
      case Node(key, pri, left, right) =>
        foreach(f, left)
        f(key, pri)
        foreach(f, right)
    }
  }

  def map[R, K, P](
    f: K => R,
    tree: Tree[K, P]
  )(implicit ordP: P => Ordered[P], ordR: R => Ordered[R]): Tree[R, P] = {
    tree match {
      case Leaf() => Leaf()
      case Node(key, pri, left, right) =>
        val (t1, t2) = (Treap.insert(f(key), pri, map(f, left)), map(f, right))
        Treap.merge(t1, t2)
    }
  }

  def filter[K, P](
    f: K => Boolean,
    tree: Tree[K, P]
  )(implicit ordK: K => Ordered[K], ordP: P => Ordered[P]): Tree[K, P] = {
    tree match {
      case Leaf() => Leaf()
      case Node(key, pri, left, right) =>
        var (t1, t2) = (Treap.filter(f, left), Treap.filter(f, right))
        if (f(key)) t1 = Treap.insert(key, pri, t1)
        Treap.merge(t1, t2)
    }
  }
}
