import scala.util.Random

class Node[T <: Ordered[T]] (
    var priority: Int,
    var key: T,
    var value: Int
) {
    var left: Option[Node[T]] = Option.empty[Node[T]]
    var right: Option[Node[T]] = Option.empty[Node[T]]
}

class Treap[T <: Ordered[T]](
    private val initList: T*
) {
    private val generator = new Random(42)
    private var root: Option[Node[T]] = Option.empty[Node[T]]

    for (x <- initList) {
        add(x)
    }

    private def recursiveForeach(v: Option[Node[T]], f: T => Unit): Unit = {
        if (v.isEmpty) {
            return
        }
        recursiveForeach(v.get.left, f)
        f(v.get.key)
        recursiveForeach(v.get.right, f)
    }

    def foreach(f: T => Unit): Unit = {
        recursiveForeach(root, f)
    }

    final def map[Q <: Ordered[Q]](f: T => Q): Treap[Q] = {
        val mapped = new Treap[Q]
        foreach(key => mapped.add(f(key)))
        mapped
    }

    final def filter(p: T => Boolean): Treap[T] = {
        val filtered = new Treap[T]
        foreach(key => if (p(key)) filtered.add(key))
        filtered
    }

    def recursiveWithFilter(v: Option[Node[T]], p: T => Boolean): Option[Node[T]] = {
        if (v.isEmpty) {
            return v
        }
        val l = recursiveWithFilter(v.get.left, p)
        val r = recursiveWithFilter(v.get.right, p)
        if (p(v.get.key)) {
            v.get.left = l
            v.get.right = r
            v
        } else {
            merge(l, r)
        }
    }

    final def withFilter(p: T => Boolean): Treap[T] = {
        root = recursiveWithFilter(root, p)
        this
    }

    /**
      * Finds intersection i.e. creates new
      * multiset which will contain all keys
      * presented in both trees.
      * Values will be the sum of values in
      * trees.
      *
      * @param that is the tree to be intersected with this tree
      *
      * @return created tree
      * */
    def &(that: Treap[T]): Treap[T] = {
        val intersection = new Treap[T]
        this.foreach(key => {
            val count = that.get(key)
            if (count > 0) {
                intersection.put(key, this.get(key) + count)
            }
        })
        intersection
    }

    /**
      * Finds union i.e. creates new
      * multiset which will contain all keys
      * presented in at least one tree.
      * Values will be the sum of values in
      * trees.
      *
      * @param that is the tree to be merged with this tree
      *
      * @return created tree
      * */
    def |(that: Treap[T]): Treap[T] = {
        val union = new Treap[T]
        this.foreach(key => union.put(key, this.get(key)))
        that.foreach(key => union.put(key, union.get(key) + that.get(key)))
        union
    }

    /**
      * Puts the given value to the given key.
      * If values is less than of equal to zero
      * then nothing given element will be completely
      * erased from the multiset
      * */
    def put(key: T, value: Int): Unit = {
        val v = lowerBound(root, key)
        if (value <= 0) {
            if (v.isDefined && v.get.key == key) {
                var (l, r) = split(root, key)
                r = cutLowest(r)
                root = merge(l, r)
            }
            return
        }
        if (v.isDefined && v.get.key == key) {
            v.get.value = value
            return
        }
        val (l, r) = split(root, key)
        val add = Option.apply(new Node[T](generator.nextInt(Int.MaxValue), key, value))
        root = merge(merge(l, add), r)
    }

    /**
      * Adds the element x to the multiset
      * */
    def add(x: T): Unit = {
        put(x, get(x) + 1)
    }

    /**
      * Removes the element from the set.
      * If there was no such an element in the multiset
      * then nothing will happen
      * */
    def remove(x: T): Unit = {
        put(x, get(x) - 1)
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
      * If the tree is empty then nothing will happen
      *
      * @param v is the root node of the given tree
      *
      * @return the root node of the tree occurred by
      * cutting the smallest key in the tree
      * */
    private def cutLowest(v: Option[Node[T]]): Option[Node[T]] = {
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
      * Merges two given trees l and r
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
      * left part will contain all keys which are less than x
      * and right part will contain all keys which are greater than or equal to x
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
