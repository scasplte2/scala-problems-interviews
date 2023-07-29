package com.rockthejvm.trees

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PathSum extends App {

  // returns true if there is a path along the tree that sums to the target
  /*
                1
              /   \
            2       6
           / \     / \
          3   4   7   8
               \
                5
     there is a path with target = 6, [1,2,3] -> true
     there is NO path with target = 7 -> false
   */

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
  val multipathTree = {
    BNode(1,
      BNode(2,
        BNode(3, BEnd, BEnd),
        BNode(4,
          BEnd,
          BNode(-1, BEnd, BEnd)
        )
      ),
      BNode(6,
        BNode(7, BEnd, BEnd),
        BNode(8, BEnd, BEnd)
      )
    )
  }

  // this is my stack recursive impl
  def hasPathSum(tree: BTree[Int], target: Int): Boolean = {
    if (tree.isLeaf && (target - tree.value) == 0) true
    else if (tree.isEmpty) false
    else List(tree.left, tree.right).exists(t => hasPathSum(t, target - tree.value))
  }

  // Daniel's stack recursive impl
  def hasPathSum_v2(tree: BTree[Int], target: Int): Boolean = {
    if (tree.isEmpty) target == 0
    else if (tree.isLeaf) tree.value == target
    else List(tree.left, tree.right).exists(t => hasPathSum(t, target - tree.value))
    // or could use
//    else if (tree.left.isEmpty) hasPathSum_v2(tree.right, target - tree.value)
//    else hasPathSum_v2(tree.left, target - tree.value)
  }

  /*
  hp([1], [6]) =
  hp([2, 6], [5 5]) =
  hp([6, 3, 4], [5, 3, 3] =
  hp([3, 4, 7, 8], [3, 3, -1, -1]) =
  true

  Complexity - O(N) time, O(N) space
  - each node is only considered once
   */
  def hasPathSum_v3(tree: BTree[Int], target: Int): Boolean = {
    @tailrec
    def hasPathSumTailrec(nodes: Queue[BTree[Int]], targets: Queue[Int]): Boolean = {
      if (nodes.isEmpty) false
      else {
        val node = nodes.head
        val targetValue = targets.head
        val children = List(node.left, node.right).filterNot(_.isEmpty)
        val childrenTargets = children.map(_ => targetValue - node.value)

        if (node.isLeaf && node.value == targetValue) true
        else hasPathSumTailrec(nodes.tail ++ children, targets.tail ++ childrenTargets)
      }
    }

    hasPathSumTailrec(Queue(tree), Queue(target))
  }

  // find ALL paths from root to leaf such that the sum of values == target
  def findSumPaths(tree: BTree[Int], target: Int): List[List[Int]] = {
    /*
                  1
                /   \
              2       6
             / \     / \
            3   4   7   8
                 \
                  5
        I want to build this up in acc
        [[1]]
        [[1,2], [1,6]]
        [[1,2,3], [1,2,4], [1,6,7], [1,6,8]]
        [[1,2,3], [1,2,4,5], [1,6,7], [1,6,8]]

        fsp([1], [[]], [[]])
        fsp([2,6], [[1], [1]], [[]]) // has two children so duplicate in buffer
        fsp([3,4,6], [[2,1], [2,1], [1]], [[]]) // 3 is a leaf so just push to head buffer and move to acc
        fsp([4,6], [[2,1], [1]], [[3,2,1]]) // only right child for a so only update head in buffer
        fsp([5,6], [[4,2,1], [1]], [[3,2,1]])
        fsp([6], [[1]], [[5,4,2,1], [3,2,1]])
        fsp([7,8], [[6,1], [6,1]], [[5,4,2,1], [3,2,1]])
        fsp([8], [[6,1]], [[7,6,1], [5,4,2,1], [3,2,1]])
        fsp([], [[]], [[8,6,1], [7,6,1], [5,4,2,1], [3,2,1]])
        [[8,6,1], [7,6,1], [5,4,2,1], [3,2,1]].filterNot(_.sum == target)
     */

    // my impl
    @tailrec
    def findSumPathsTailrec(nodes: List[BTree[Int]], buffer: List[List[Int]] = List(), acc: List[List[Int]] = List()): List[List[Int]] = {
      if (nodes.isEmpty) acc.filter(_.sum == target)
      else {
        val node = nodes.head

        if (node.isEmpty) findSumPathsTailrec(nodes.tail, buffer, acc)
        else if (node.isLeaf) findSumPathsTailrec(nodes.tail, buffer.tail, (node.value :: buffer.head) :: acc)
        else if (node.left.isEmpty) findSumPathsTailrec(node.right :: nodes.tail, (node.value :: buffer.head) :: buffer.tail, acc)
        else if (node.right.isEmpty) findSumPathsTailrec(node.left :: nodes.tail, (node.value :: buffer.head) :: buffer.tail, acc)
        else {
          val newNodes = node.left :: node.right :: nodes.tail
          val newBuffer =
            if (buffer.isEmpty) List(node.value) :: List(node.value) :: buffer
            else (node.value :: buffer.head) :: (node.value :: buffer.head) :: buffer.tail

          findSumPathsTailrec(newNodes, newBuffer, acc)
        }
      }
    }

    // daniel's impl
    def stackPaths(tree: BTree[Int], currentTarget: Int): List[List[Int]] = {
      if (tree.isEmpty) List()
      else if (tree.isLeaf)
        if (currentTarget == tree.value) List(List(tree.value))
        else List()
      else List(tree.left, tree.right).filterNot(_.isEmpty).flatMap { childNode =>
        val subPaths = stackPaths(childNode, currentTarget - tree.value)
        subPaths.map(path => tree.value :: path)
      }
    }

    @tailrec
    def tailPaths(
        nodes: List[BTree[Int]],
        targets: List[Int],
        currentPath: List[BTree[Int]],
        expanded: Set[BTree[Int]],
        acc: List[List[Int]]
                 ): List[List[Int]] = {
      if (nodes.isEmpty) acc
      else {
        val node = nodes.head
        val currentTarget = targets.head
        val children = List(node.left, node.right).filterNot(_.isEmpty)
        val childTargets = children.map(_ => currentTarget - node.value)

        if (node.isLeaf)
          if (node.value == currentTarget)
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, (node :: currentPath).reverse.map(_.value) :: acc)
          else {
            tailPaths(nodes.tail, targets.tail, currentPath, expanded, acc)
          }
        else {
          if (expanded.contains(node)) tailPaths(nodes.tail, targets.tail, currentPath.tail, expanded, acc)
          else {
            tailPaths(children ++ nodes, childTargets ++ targets, node :: currentPath, expanded + node, acc)
          }
        }
      }
    }



    findSumPathsTailrec(List(tree))
//    findSumStackrec(tree, target)
  }

  println(hasPathSum(danielTree, 6)) //true
  println(hasPathSum(danielTree, 7)) // false
  println(hasPathSum(danielTree, 8)) // false
  println(hasPathSum(danielTree, 12)) // true
  println(hasPathSum(danielTree, 15)) // true

  println("\n")
  println(hasPathSum_v3(danielTree, 6)) //true
  println(hasPathSum_v3(danielTree, 7)) // false
  println(hasPathSum_v3(danielTree, 8)) // false
  println(hasPathSum_v3(danielTree, 12)) // true
  println(hasPathSum_v3(danielTree, 15)) // true

  println("\n")
  println(findSumPaths(multipathTree, 6)) //true
  println(findSumPaths(danielTree, 7)) // false
  println(findSumPaths(danielTree, 8)) // false
  println(findSumPaths(danielTree, 12)) // true
  println(findSumPaths(danielTree, 15)) // true

}
