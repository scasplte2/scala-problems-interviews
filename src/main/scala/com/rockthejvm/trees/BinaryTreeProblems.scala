package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

sealed abstract class BTree[+T]{
  def value: T
  def left: BTree[T]
  def right: BTree[T]
  def isEmpty: Boolean

  /**
    * Easy problems
    */
  def isLeaf: Boolean

  def collectLeaves: List[BTree[T]]

  def leafCount: Int

  // the number of nodes in the tree
  def size: Int

  /**
    * Medium difficulty problems
    */
  // nodes at a given level
  def collectNodes(level: Int): List[BTree[T]]

  // mirror a tree
  def mirror: BTree[T]

  // returns true if the shape of the two tree is the same
  // **the values do not have to be the same
  def sameShapeAs[S >: T](that: BTree[S]): Boolean

  // test whether the tree is symmetric with respect to the root node
  def isSymmetric: Boolean

  // collect all nodes to a list
  def toList: List[T]
}

object BTree {
  def apply[T](): BTree[T] = BEnd
  def apply[T](value: T): BTree[T] = BNode(value, BEnd, BEnd)
  def apply[T](value: T, left: BTree[T], right: BTree[T]): BTree[T] = BNode(value, left, right)
}

case object BEnd extends BTree[Nothing] {
  // these are side effecting, for functional impl would want to return Options
  override def value: Nothing = throw new NoSuchElementException

  override def left: BTree[Nothing] = throw new NoSuchElementException

  override def right: BTree[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = false

  override def collectLeaves: List[BTree[Nothing]] = List()

  override def leafCount: Int = 0

  override def size: Int = 0

  /**
    * Medium difficulty problems
    */
  override def collectNodes(level: Int): List[BTree[Nothing]] = List()

  override def mirror: BTree[Nothing] = BEnd

  override def sameShapeAs[S >: Nothing](that: BTree[S]): Boolean = that.isEmpty

  override def isSymmetric: Boolean = true

  override def toList: List[Nothing] = List()
}

case class BNode[+T]( override val value: T,
                      override val left: BTree[T],
                      override val right: BTree[T]) extends BTree[T] {
  override def isEmpty: Boolean = false

  /**
    * Easy problems
    */
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def collectLeaves: List[BTree[T]] = {
    /*
                 ___10___
                /        \
              20         21
             /  \      /   \
            30  _|_  _|_   _|_
           /  \
         _|_  _|_

       clt([10], [])
      = clt([20,21], [])
      = clt([30,_|_,21], [])
      = clt([_|_,21], [30])
      = clt([21], [30])
      = clt([], [21,30])
     */
    @tailrec
    def collectLeavesTailrec(remaining: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
      if (remaining.isEmpty) acc
      else if (remaining.head.isEmpty) collectLeavesTailrec(remaining.tail, acc)
      else if (remaining.head.isLeaf) collectLeavesTailrec(remaining.tail, remaining.head :: acc)
      else {
        val node = remaining.head
        collectLeavesTailrec(node.left :: node.right :: remaining.tail, acc)
      }
    }

    collectLeavesTailrec(List(this), List())
  }

  override def leafCount: Int = collectLeaves.length

  // could also have done
  // val size: Int = 1 + left.size + right.size
  // since the val is computed at construction time
  override def size: Int = {
    @tailrec
    def sizeTailrec(remaining: List[BTree[T]], count: Int): Int = {
      if (remaining.isEmpty) count
      else if (remaining.head.isEmpty) sizeTailrec(remaining.tail, count)
      else {
        val node = remaining.head
        sizeTailrec(node.left :: node.right :: remaining.tail, count + 1)
      }
    }

    sizeTailrec(List(this), 0)
  }

  /**
    * Medium difficulty problems
    */
  override def collectNodes(level: Int): List[BTree[T]] = {
    /*
                     ___10___
                    /        \
                  20         21
                 /  \      /   \
                30  _|_  _|_   _|_
               /  \
             _|_  _|_

           collectNodes(2) = cntr(0, [10])
          = cntr(1, [20, 21])
          = cntr(2, [30])
         */
    @tailrec
    def collectNodesTailrec(depth: Int, currentNodes: List[BTree[T]]): List[BTree[T]] = {
      if (currentNodes.isEmpty) List()
      else if (depth == level) currentNodes
      else {
        val expandedNodes = for {
          node <- currentNodes
          child <- List(node.left, node.right) if !child.isEmpty
        } yield child

        collectNodesTailrec(depth + 1, expandedNodes)
      }
    }

    if (level < 0) List()
    else collectNodesTailrec(0, List(this))
  }

  override def mirror: BTree[T] = {
    /*
                        ___10___
                       /        \
                     20         21
                    /  \      /   \
                   30  _|_  _|_   _|_
                  /  \
                _|_  _|_

      mirrorTailrec([10], [], [])
     = mtr([20, 21, 10], [10], [])
     = mtr([30, _|_, 20, 21, 10], [20, 10], [])
     = mtr([_|_, 20, 21, 10], [20, 10], [30]) // can push 30 to done since it is a Leaf
     = mtr([20, 21, 10], [20, 10], [_|_, 30])
     = mtr([21, 10], [20, 10], [{20 -> (_|_, 30)] // flip around the 20 mode because it is in visited and rem
     = mtr([10], [20, 10], [21, {20 -> (_|_, 30)]) // again push 21 to done since it is a leaf
     = mtr([], [20, 10], [{10 -> (21, {20 -> (_|_, 30)})}]
     = [{10 -> (21, {20 -> (_|_, 30)})}] or
                          ___10___
                         /        \
                        21         20
                       /  \      /    \
                     _|_  _|_  _|_    30
                                     /  \
                                   _|_  _|_
    */
    @tailrec
    def mirrorTailrec(remaining: List[BTree[T]], expanded: Set[BTree[T]], acc: List[BTree[T]]): BTree[T] = {
      if (remaining.isEmpty) acc.head
      else {
        val node = remaining.head
        if (node.isLeaf || node.isEmpty) mirrorTailrec(remaining.tail, expanded, node :: acc)
        else if (expanded.contains(node)) {
          val newNode = BNode(node.value, acc.head, acc.tail.head)
          val newAcc = newNode :: acc.tail.tail
          mirrorTailrec(remaining.tail, expanded, newAcc)
        } else {
          mirrorTailrec(node.left :: node.right :: remaining, expanded + node, acc)
        }
      }
    }

    mirrorTailrec(List(this), Set(), List())
  }

  override def sameShapeAs[S >: T](that: BTree[S]): Boolean = {
    @tailrec
    def sameShapeAsTailrec(thisRemaining: List[BTree[S]], thatRemaining: List[BTree[S]]): Boolean = {
      if (thisRemaining.isEmpty) thatRemaining.isEmpty
      else if (thatRemaining.isEmpty) thisRemaining.isEmpty
      else {
        val thisNode = thisRemaining.head
        val thatNode = thatRemaining.head

        if (thisNode.isEmpty) thatNode.isEmpty && sameShapeAsTailrec(thisRemaining.tail, thatRemaining.tail)
        else if (thisNode.isLeaf) thatNode.isLeaf && sameShapeAsTailrec(thisRemaining.tail, thatRemaining.tail)
        else {
          sameShapeAsTailrec(
            thisNode.left :: thisNode.right :: thisRemaining.tail,
            thatNode.left :: thatNode.right :: thatRemaining.tail
          )
        }
      }
    }

    sameShapeAsTailrec(List(this), List(that))
  }

  override def isSymmetric: Boolean = this.left.sameShapeAs(this.right)

/*
                     ___10___
                    /        \
                  20         21
                 /  \      /   \
                30  _|_  _|_   _|_
               /  \
             _|_  _|_
*/
// traversal options
// - pre-order: root, left, right -> [10,20,30,21]
// - in-order: left, root, right -> [30,20,10,21]
// - post-order: left, right, root -> [30,20,21,10]
// - per-level: collect at a given level -> [10, 20, 21, 30]
override def toList: List[T] = {

  // my impls
  @tailrec
  def toPreOrderListTailrec(remaining: List[BTree[T]], expanded: Set[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
    if (remaining.isEmpty) acc
    else {
      val node = remaining.head
      if (node.isEmpty || expanded.contains(node)) toPreOrderListTailrec(remaining.tail, expanded, acc)
      else {
        val newRemaining = node.right :: node.left :: remaining.tail
        val newVisited = expanded + node
        val newAcc = node :: acc
        toPreOrderListTailrec(newRemaining, newVisited, newAcc)
      }
    }
  }

  @tailrec
  def toPostOrderListTailrec(remaining: List[BTree[T]], expanded: Set[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = {
    if (remaining.isEmpty) acc
    else {
      val node = remaining.head
      if (node.isEmpty || expanded.contains(node)) toPostOrderListTailrec(remaining.tail, expanded, acc)
      else {
        val newRemaining = remaining.tail ++ List(node.right, node.left)
        val newVisited = expanded + node
        val newAcc = node :: acc
        toPostOrderListTailrec(newRemaining, newVisited, newAcc)
      }
    }
  }

  // Daniel's impl
  // this is a stack recursive ordering
  def preOrderStack(tree: BTree[T]): List[T] = {
    if (tree.isEmpty) List()
    else tree.value :: preOrderStack(tree.left) ++ preOrderStack(tree.right)
  }

  /*
  // for pre-order: expand the children into the stack, if (in stack and visited) or leaf, push to acc
  pot([10], [], []) =
  pot([10, 20, 21], [10], []) =
  pot([20, 21], [10], [10]) =
  pot([20, 30, 21], [10, 20], [10]) =
  pot([30, 21], [10, 20], [10, 20]) =
  pot([21], [10, 20], [10, 20, 30]) =
  pot([], [10, 20], [10, 20, 30, 21])
  = [10, 20, 30, 21]
   */
  @tailrec
  def preOrderTailrec_v2(stack: List[BTree[T]], visited: Set[BTree[T]] = Set(), acc: Queue[T] = Queue()): List[T] = {
    if (stack.isEmpty) acc.toList
    else {
      val node = stack.head
      if (node.isEmpty) preOrderTailrec_v2(stack.tail, visited, acc)
      else if (visited.contains(node) || node.isLeaf) preOrderTailrec_v2(stack.tail, visited, acc :+ node.value)
      else preOrderTailrec_v2(node :: List(node.left, node.right) ++ stack.tail, visited + node, acc)
    }
  }

  @tailrec
  def inOrderTailrec_v2(stack: List[BTree[T]], visited: Set[BTree[T]] = Set(), acc: Queue[T] = Queue()): List[T] = {
    if (stack.isEmpty) acc.toList
    else {
      val node = stack.head
      if (node.isEmpty) inOrderTailrec_v2(stack.tail, visited, acc)
      else if (visited.contains(node) || node.isLeaf) inOrderTailrec_v2(stack.tail, visited, acc :+ node.value)
      else inOrderTailrec_v2(node.left :: node :: node.right :: stack.tail, visited + node, acc)
    }
  }

  @tailrec
  def postOrderTailrec_v2(stack: List[BTree[T]], visited: Set[BTree[T]] = Set(), acc: Queue[T] = Queue()): List[T] = {
    if (stack.isEmpty) acc.toList
    else {
      val node = stack.head
      if (node.isEmpty) postOrderTailrec_v2(stack.tail, visited, acc)
      else if (visited.contains(node) || node.isLeaf) postOrderTailrec_v2(stack.tail, visited, acc :+ node.value)
      else postOrderTailrec_v2(node.left :: node.right :: node :: stack.tail, visited + node, acc)
    }
  }

  @tailrec
  def perLevelTailrec(level: List[BTree[T]], acc: Queue[BTree[T]] = Queue()): List[T] = {
    if (level.isEmpty) acc.map(_.value).toList
    else {

      // same as the for-comprehension below
//      val newLevel = level.flatMap { node =>
//        if (node.isLeaf) List()
//        else if (node.left.isEmpty) List(node.right)
//        else if (node.right.isEmpty) List(node.left)
//        else List(node.left, node.right)
//      }

      // another option for expansion
//      val newLevel = level.flatMap(node => List(node.left, node.right).filterNot(_.isEmpty))

      val newLevel = for {
        node <- level
        child <- List(node.left, node.right) if (!child.isEmpty)
      } yield child

      perLevelTailrec(newLevel, acc ++ level)
    }
  }


  //toPreorderListTailrec(List(this), Set(), List()).map(_.value)
  //toPostOrderListTailrec(List(this), Set(), List()).map(_.value)
//  preOrderTailrec_v2(List(this))
//  inOrderTailrec_v2(List(this))
//  postOrderTailrec_v2(List(this))
  perLevelTailrec(List(this))
}
}

object BinaryTreeProblems extends App {

val smallTree = BNode(10, BNode(20, BNode(30, BEnd, BEnd), BEnd), BNode(21, BEnd, BEnd))
val danielTree = {
  BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4,
        BEnd,
        BNode(5, BEnd, BEnd)
      )
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )
}
val degenerateTree = (1 to 100000).foldLeft(BNode(0, BEnd, BEnd)){ case (acc, int) => BNode(int, acc, BEnd)}
val symTree = {
  BNode(1,
    BNode(2,
      BNode(3, BEnd, BEnd),
      BNode(4, BEnd, BEnd)
    ),
    BNode(6,
      BNode(7, BEnd, BEnd),
      BNode(8, BEnd, BEnd)
    )
  )
}

def testEasyProblems(): Unit = {
  println(danielTree.collectLeaves)
  println(danielTree.leafCount)
  println(danielTree.size)
  println(degenerateTree.size)
}
//  testEasyProblems()

def testMediumProblems(): Unit = {
  println(danielTree.collectNodes(2).map(_.value))
  println(danielTree.collectNodes(1).map(_.value))
  println(danielTree.collectNodes(0).map(_.value))
  println(danielTree.collectNodes(42).map(_.value))

  println(smallTree)
  println(smallTree.mirror)

  println(smallTree.sameShapeAs(danielTree))
  println(smallTree.sameShapeAs(smallTree))

  println(smallTree.isSymmetric)
  println(symTree.isSymmetric)

  println(smallTree.toList)
}
testMediumProblems()

}
