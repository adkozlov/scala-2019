package ru.spbau.jvm.scala.MultiSet

/**
 * Implementation of MultiSet using Splay-tree.
 *
 * Values stored must be ordered.
 * MultiSet does not track element changes that are done by user.
 *    So if a binary tree invariant is broken (in MultiSet function or not)
 *    the behaviour is undefined.
 * If MultiSet is changed inside foreach/map/filter the behaviour is undefined.
 */
class MultiSet[T <: Ordered[T]] {
  private class SplayTreeNode(val value:       T,
                              var occurrences: Int,
                              var parent:      Option[SplayTreeNode],
                              var left:        Option[SplayTreeNode],
                              var right:       Option[SplayTreeNode]) {

    def isRoot: Boolean = parent.isEmpty

    def isLeftSon: Boolean = eq(parent.get.left.orNull)

    def zig(): Unit = {
      val previousParent = parent.get
      if (isLeftSon) {
        previousParent.left = right
        if (right.isDefined) {
          right.get.parent = Some(previousParent)
        }
        right = Some(previousParent)
      } else {
        previousParent.right = left
        if (left.isDefined) {
          left.get.parent = Some(previousParent)
        }
        left = Some(previousParent)
      }
      parent = previousParent.parent
      if (!previousParent.isRoot) {
        if (previousParent.isLeftSon) {
          parent.get.left = Some(this)
        }
        else {
          parent.get.right = Some(this)
        }
      }
      previousParent.parent = Some(this)
    }

    def doubleZig(): Unit = {
      if (isLeftSon == parent.get.isLeftSon) {
        zigZig()
      } else {
        zigZag()
      }
    }

    def zigZig(): Unit = {
      parent.get.zig()
      zig()
    }

    def zigZag(): Unit = {
      zig()
      zig()
    }

    @scala.annotation.tailrec
    final def splay(): Unit = {
      if (isRoot) {
        return
      }
      if (parent.get.isRoot) {
        // Depth is 1
        zig()
      } else {
        // Depth is >= 2
        doubleZig()
      }
      splay()
    }

    /** Returns first node in subtree of this node and splays it */
    def firstInSubtree(): SplayTreeNode = {
      if (left.isDefined) {
        left.get.firstInSubtree()
      } else {
        this.splay()
        this
      }
    }

    /** Returns last node in subtree of this node and splays it */
    def lastInSubtree(): SplayTreeNode = {
      if (right.isDefined) {
        right.get.lastInSubtree()
      } else {
        this.splay()
        this
      }
    }

    /** Returns next node if exists, None otherwise */
    def next(): Option[SplayTreeNode] = {
      splay()
      if (right.isDefined) {
        Some(right.get.firstInSubtree())
      } else {
        None
      }
    }
  }

  private var root: Option[SplayTreeNode] = None
  private var _size: Int = 0

  /** Returns number of different values in a MultiSet */
  def size(): Int = _size

  /**
   * Merges two trees.
   *
   * Values in treeA must be strictly less than values in treeB
   */
  private def merge(rootA: Option[SplayTreeNode], rootB: Option[SplayTreeNode]): Option[SplayTreeNode] = {
    if (rootA.isEmpty) {
      return rootB
    }
    rootA.get.lastInSubtree()
    rootA.get.right = rootB
    if (rootB.isDefined) {
      rootB.get.parent = rootA
    }
    rootA
  }

  /**
   * If node with {@code value} is presented in subtree, returns corresponding node.
   * Otherwise returns leaf that should be connected with this value
   */
  @scala.annotation.tailrec
  private def find(root: SplayTreeNode, value: T): SplayTreeNode = {
    if (root.value == value) {
      return root
    }
    val nextNode = if (root.value < value) root.right else root.left
    if (nextNode.isEmpty) {
      root
    } else {
      find(nextNode.get, value)
    }
  }

  /**
   * Adds {@code value} {@code occurrences} times to the tree.
   *
   * @param value must be not null
   * @param occurrences positive integer
   */
  private def add(value: T, occurrences: Int = 1): Unit = {
    if (occurrences <= 0) {
      throw new IllegalArgumentException(
        "Multiset.add got not positive occurrences as parameter")
    }
    if (value == null) {
      throw new IllegalArgumentException("Multiset.add got null as parameter")
    }
    if (root.isEmpty) {
      _size += 1
      root = Some(new SplayTreeNode(value, occurrences, None, None, None))
      return
    }
    val node = find(root.get, value)
    if (node.value == value) {
      // value is already presented in multiset
      node.occurrences += occurrences
    } else {
      // value is not presented in multiset
      _size += 1
      val newNode = Some(new SplayTreeNode(value, occurrences, Some(node), None, None))
      if (node.value < value) {
        node.right = newNode
      } else {
        node.left = newNode
      }
      node.splay()
      newNode.get.splay()
      root = newNode
    }
  }

  def put(value: T): Unit = {
    if (value == null) {
      throw new IllegalArgumentException("Multiset.put got null as parameter")
    }
    add(value)
  }

  /** Returns number of occurrences of given value */
  def count(value: T): Int = {
    if (value == null) {
      throw new IllegalArgumentException("Multiset.count got null as parameter")
    }
    if (root.isEmpty) {
      return 0
    }
    val node = find(root.get, value)
    node.splay()
    root = Some(node)
    if (node.value == value) {
      node.occurrences
    } else {
      0
    }
  }

  /** Alias for count */
  def apply(value: T): Int = count(value)

  /** Returns minimal value in a MultiSet */
  def firstValue(): Option[T] = {
    if (root.isEmpty) {
      return None
    }
    root = Some(root.get.firstInSubtree())
    Some(root.get.value)
  }

  /** Returns maximal value in a MultiSet */
  def lastValue(): Option[T] = {
    if (root.isEmpty) {
      return None
    }
    root = Some(root.get.lastInSubtree())
    Some(root.get.value)
  }

  /**
   * Moves root to next value in a tree.
   *
   * @return next value if exists, None otherwise
   */
  private def rootToNext(): Option[T] = {
    if (root.isEmpty) {
      return None
    }
    val node = root.get.next()
    if (node.isDefined) {
      root = node
      Some(node.get.value)
    } else {
      None
    }
  }

  /**
   * Calculates intersection of two MultiSets.
   *
   * If element is in both MultiSets, it will be presented in result MultiSet
   *   as many times as at was presented in bots MultiSets in total.
   *   For instance if element 42 occurred in first MultiSet 2 times and
   *   in second MultiSet 3 times in resulted MultiSet it will be 5 times
   */
  def &(other: MultiSet[T]): MultiSet[T] = {
    var currentValue = firstValue()
    var otherCurrentValue = other.firstValue()
    val newMultiSet = new MultiSet[T]()
    while (currentValue.isDefined && otherCurrentValue.isDefined) {
      if (currentValue.get == otherCurrentValue.get) {
        newMultiSet.add(currentValue.get, root.get.occurrences + other.root.get.occurrences)
        currentValue = rootToNext()
        otherCurrentValue = rootToNext()
      } else {
        if (currentValue.get < otherCurrentValue.get) {
          currentValue = rootToNext()
        } else {
          otherCurrentValue = other.rootToNext()
        }
      }
    }
    newMultiSet
  }

  /** Calculates union of MultiSets */
  def |(other: MultiSet[T]): MultiSet[T] = {
    val newMultiSet = new MultiSet[T]()
    traversal(node => newMultiSet.add(node.value, node.occurrences))
    other.traversal(node => newMultiSet.add(node.value, node.occurrences))
    newMultiSet
  }

  /**
   * Invokes function on every element of a tree in ascending order
   *
   * Implemented without using usual tree traversal because of splay-tree features
   * For instance if function calls some MultiSet function inside it, it may broke
   *   tree structure while we are inside of dfs
   */
  private def traversal(function: SplayTreeNode => Unit): Unit = {
    if (root.isEmpty) {
      return
    }
    firstValue()
    var node = root
    while (node.isDefined) {
      root = node
      function(node.get)
      node = node.get.next()
    }
  }

  /** Deletes parent of node if node is presented */
  private def deleteParent(node: Option[SplayTreeNode]): Unit = {
    if (node.isDefined) {
      node.get.parent = None
    }
  }

  /**
   * Removes {@code occurrences} occurrences of {@code value} from MultiSet
   *
   * If {@code occurrences} is not specified, removes all occurrences.
   * If there are no {@code occurrences} occurrences of {@code value} then
   *   removes all that are presented
   *
   * @param occurrences positive number
   *
   * @return number of occurrences that were removed
   */
  def remove(value: T, occurrences: Int = Int.MaxValue): Int = {
    if (value == null) {
      throw new IllegalArgumentException("Multiset.remove got null as parameter")
    }
    if (occurrences <= 0) {
      throw new IllegalArgumentException("Multiset.remove got not positive occurrences")
    }
    val removed = Math.min(occurrences, count(value))
    if (removed > 0) {
      // It is guaranteed that root is needed node
      root.get.occurrences -= removed
      if (root.get.occurrences == 0) {
        _size -= 1
        deleteParent(root.get.left)
        deleteParent(root.get.right)
        root = merge(root.get.left, root.get.right)
      }
    }
    removed
  }

  def foreach(function: T => Unit): Unit = {
    traversal(node => function(node.value))
  }

  def filter(predicate: T => Boolean): MultiSet[T] = {
    val newMultiSet = new MultiSet[T]()
    traversal(node => if (predicate(node.value)) newMultiSet.add(node.value, node.occurrences))
    newMultiSet
  }

  def withFilter(predicate: T => Boolean): MultiSet[T] = {
    traversal(node => if (!predicate(node.value)) remove(node.value))
    this
  }

  def map[U <: Ordered[U]](function: T => U): MultiSet[U] = {
    val newMultiSet = new MultiSet[U]()
    traversal(node => newMultiSet.add(function(node.value), node.occurrences))
    newMultiSet
  }

  override def toString: String = {
    val stringBuilder = new StringBuilder
    stringBuilder.append('[')
    traversal(node => {
      stringBuilder.append(s"${node.value} -> ${node.occurrences}, ")
    })
    if (size() > 0) {
      stringBuilder.delete(stringBuilder.length() - 2, stringBuilder.length())
    }
    stringBuilder.append(']')
    stringBuilder.toString()
  }

  def this(values: T*) {
    this()
    for (value <- values) {
      add(value)
    }
  }
}
