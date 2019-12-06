import scala.util.Random

case class Node[T <: Ordered[T]] (
    var priority: Int,
    var key: T,
) {
    var left: Option[Node[T]] = Option.empty[Node[T]]
    var right: Option[Node[T]] = Option.empty[Node[T]]
    var value = 1
}

class Treap[T <: Ordered[T]](
    private val initList: T*
) {
    private val generator = new Random(42)
    private var root: Option[Node[T]] = Option.empty[Node[T]]

    for (x <- initList) {
        add(x)
    }

    /**
      * Adds the element x to the multiset
      * */
    def add(x: T): Unit = {
        val v = lowerBound(root, x)
        if (v.isDefined) {
            v.get.value += 1
            return
        }
        val (l, r) = split(root, x)
        val add = Option.apply(new Node[T](generator.nextInt(Int.MaxValue), x))
        root = merge(merge(l, add), r)
    }

    /**
      * Removes the element from the set.
      * If there was no such an element in the multiset
      * than nothing will happen
      * */
    def remove(x: T): Unit = {
        val v = lowerBound(root, x)
        if (v.isEmpty) {
            return
        }
        v.get.value -= 1
        if (v.get.value > 0) {
            return
        }
        var (l, r) = split(root, x)
        r = cutLowest(r)
        root = merge(l, r)
    }

    /**
      * Finds the number of occurrences of the given element x
      * in the multiset
      *
      * @return the number of occurrences
      * */
    def get(x: T): Int = {
        val v = lowerBound(root, x)
        if (v.isEmpty || v.get.key != x) 0 else v.get.value
    }

    /**
      * Cuts the lowest key in the given tree.
      * If the tree is empty than nothing will happen
      *
      * @param v is the root node of the given tree
      *
      * @return the root node of the tree occurred by
      * cutting the smallest key in the tree
      * */
    def cutLowest(v: Option[Node[T]]): Option[Node[T]] = {
        if (v.isEmpty) {
            return v
        }
        if (v.get.left.isEmpty) {
            val root = v.get.right
            v.get.right = Option.empty[Node[T]]
            return root
        }
        val root = v
        var u = v
        var pu = Option.empty[Node[T]]
        while (u.get.left.isDefined) {
            pu = u
            u = u.get.left
        }
        pu.get.left = u.get.right
        u.get.right = Option.empty[Node[T]]
        root
    }

    /**
      * Merges two given trees l and r taking into account
      * supposing that given trees are correct and all keys
      * in the left tree are less than all keys in the right tree
      * */
    private def merge(l: Option[Node[T]], r: Option[Node[T]]): Option[Node[T]] = {
        if (l.isEmpty || r.isEmpty) {
            return if (l.isEmpty) r else l
        }
        if (l.get.priority < r.get.priority) {
            val merged = merge(l.get.right, r)
            l.get.right = merged
            l
        } else {
            val merged = merge(l, r.get.left)
            r.get.left = merged
            r
        }
    }

    /**
      * Finds the node which contain smallest value among all nodes
      * with values greater or equal to x
      *
      * @param v is the root node of the tree to be processed
      * @param x is the delimiter
      *
      * @return founded tree or Option.empty if there is no such a node
      * */
    private def lowerBound(v: Option[Node[T]], x: T): Option[Node[T]] = {
        if (v.isEmpty) {
            return v
        }
        if (v.get.key < x) {
            lowerBound(v.get.right, x)
        } else {
            var result = lowerBound(v.get.left, x)
            if (result.isEmpty) {
                result = v
            }
            result
        }
    }

    /**
      * Splits the tree v by delimiter x in such a way that
      * left part will contain all keys which are less then x
      * and right part will contain all keys which are greater or equal to x
      *
      * @param v is the tree to be splitted
      * @param x is the delimiter
      *
      * @return pair of trees obtained by splitting
      * */
    private def split(v: Option[Node[T]], x: T): (Option[Node[T]], Option[Node[T]]) = {
        if (v.isEmpty) {
            return (Option.empty[Node[T]], Option.empty[Node[T]])
        }
        if (v.get.key < x) {
            val (l, r) = split(v.get.right, x)
            v.get.right = l
            (v, r)
        } else {
            val (l, r) = split(v.get.left, x)
            v.get.left = r
            (l, v)
        }
    }
}
