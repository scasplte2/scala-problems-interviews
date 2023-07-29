package com.rockthejvm.trees

import scala.annotation.tailrec

object CameraSurveillance extends App {

  /*
  Given a binary tree, we install cameras on the nodes of the tree.
  Each camera at a node can monitors its parent, itself, and its immediate children.
  Calculate the minimum number of cameras needed to monitor all nodes of the tree

  Ex.
      1
     / \
    2   3
   /
  4
  => 2

  Could mount at 1 & 2 or 3 & 2 or 4 & 1
  Many possible solutions but concerned with returning the minimum number not the positions

  Ex. 2
             __ 1 ___
           /         \
          2           3
         / \         / \
        4   5       6   7
       / \   \     / \   \
      8  9   10   11 12   13
     /                \
    14                 15

   => 7


   */
  def cameraSurveillance[T](tree: BTree[T]): Int = {
    @tailrec
    def findDepthTailrec(nodes: List[BTree[T]], buffer: List[List[T]] = List(), acc: List[List[T]] = List()): Int = {
      if (nodes.isEmpty) acc.map(_.length).max
      else {
        val node = nodes.head

        if (node.isEmpty) findDepthTailrec(nodes.tail, buffer, acc)
        else if (node.isLeaf) findDepthTailrec(nodes.tail, buffer.tail, (node.value :: buffer.head) :: acc)
        else if (node.left.isEmpty) findDepthTailrec(node.right :: nodes.tail, (node.value :: buffer.head) :: buffer.tail, acc)
        else if (node.right.isEmpty) findDepthTailrec(node.left :: nodes.tail, (node.value :: buffer.head) :: buffer.tail, acc)
        else {
          val newNodes = node.left :: node.right :: nodes.tail
          val newBuffer =
            if (buffer.isEmpty) List(node.value) :: List(node.value) :: buffer
            else (node.value :: buffer.head) :: (node.value :: buffer.head) :: buffer.tail

          findDepthTailrec(newNodes, newBuffer, acc)
        }
      }
    }

    val depth = findDepthTailrec(List(tree))
    val oddLevels = (0 until depth).filter(_ % 2 != 0).flatMap(tree.collectNodes).length
    val evenLevels = (0 until depth).filter(_ % 2 == 0).flatMap(tree.collectNodes).length

    Math.min(oddLevels, evenLevels)
  }

  def cameraSurveillance_v2[T](tree: BTree[T]): Int = {
    val COVERED = 0
    val NOT_COVERED = 1
    val CAMERA = 2

    def minCamerasStack(node: BTree[T]): (Int, Int) = {
      if (node.isEmpty) (0, COVERED)
      else {
        val (leftNumCameras, leftState) = minCamerasStack(node.left)
        val (rightNumCameras, rightState) = minCamerasStack(node.right)

        /*
        - left or right is NOT COVERED => place camera in this node
        - left or right has CAMERA => consider the node covered
        - consider the node NOT COVERED
         */
        if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
        else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
        else (leftNumCameras + rightNumCameras, NOT_COVERED)
      }
    }

      val (stackNumCameras, stackRootState) = minCamerasStack(tree)
      if (stackRootState == NOT_COVERED) stackNumCameras + 1
      else stackNumCameras
    }

  def cameraSurveillance_v3[T](tree: BTree[T]): Int = {
    val COVERED = 0
    val NOT_COVERED = 1
    val CAMERA = 2

    /*
           1
          / \
         2   3
        /
       4

    mctr([1], [], [])
    mctr([2,3,1], [1], [])
    mctr([4,_|_,2,3,1], [1,2], [])
    mctr([_|_,_|_,4,_|_,2,3,1], [1,2], [])
    mctr([_|_,4,_|_,2,3,1], [1,2,4], [(0, COV)])
    mctr([4,_|_,2,3,1], [1,2,4], [(0, COV), (0, COV)])
    mctr([_|_,2,3,1], [1,2,4], [(0, NOT)])
    mctr([2,3,1], [1,2,4], [(0, COV), (0, NOT)])
    mctr([3,1], [1,2,4], [(1, CAM)])
    mctr([_|_,_|_,3,1], [1,2,4,3], [(1, CAM)])
    mctr([_|_,3,1], [1,2,4,3], [(0, COV), (1, CAM)])
    mctr([3,1], [1,2,4,3], [(0, COV), (0, COV), (1, CAM)])
    mctr([1], [1,2,4,3], [(0, NOT), (1, CAM)])
    mctr([], [1,2,4,3], [(2,CAM)])
    = 2
     */
    @tailrec
    def minCamerasTailrec(stack: List[BTree[T]], visited: Set[BTree[T]], coverageStack:List[(Int, Int)]): (Int, Int) = {
      if (stack.isEmpty) coverageStack.head
      else {
        val node = stack.head

        if (node.isEmpty) {
          minCamerasTailrec(stack.tail, visited, (0, COVERED) :: coverageStack)

        } else if (visited.contains(node)) {
          val (leftNumCameras, leftState) = coverageStack.head
          val (rightNumCameras, rightState) = coverageStack.tail.head

          val parentState =
            if (leftState == NOT_COVERED || rightState == NOT_COVERED) (leftNumCameras + rightNumCameras + 1, CAMERA)
            else if (leftState == CAMERA || rightState == CAMERA) (leftNumCameras + rightNumCameras, COVERED)
            else (leftNumCameras + rightNumCameras, NOT_COVERED)

          minCamerasTailrec(stack.tail, visited, parentState :: coverageStack.tail.tail)

        } else {
          minCamerasTailrec(node.left :: node.right :: stack, visited + node, coverageStack)
        }
      }
    }


    val (tailNumCameras, tailRootState) = minCamerasTailrec(List(tree), Set(), List())
    if (tailRootState == NOT_COVERED) tailNumCameras + 1
    else tailNumCameras
  }


/*
       1
      / \
     2   3
    /
   4
 */
  val smallTree = {
    BTree(1,
      BTree(2,
        BTree(4),
        BTree()
      ),
      BTree(3)
    )
  }

  /*
      Ex. 2
             __ 1 ___
           /         \
          2           3
         / \         / \
        4   5       6   7
       / \   \     / \   \
      8  9   10   11 12   13
     /                \
    14                 15
   */
  val cameraTree = {
    BNode(1,
      BNode(2,
        BNode(4,
          BNode(8,
            BNode(14, BEnd, BEnd),
            BEnd
          ),
          BNode(9, BEnd, BEnd)
        ),
        BNode(5,
          BNode(10, BEnd, BEnd),
          BEnd)
        ),
      BNode(3,
        BNode(6,
          BNode(11, BEnd, BEnd),
          BNode(12,
            BEnd,
            BNode(15, BEnd, BEnd)
          )
        ),
        BNode(7,
          BEnd,
          BNode(13, BEnd, BEnd))
        )
      )
  }

  /*
        1
      /   \
    2       6
   / \     / \
  3   4   7   8
       \
        5
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

  println("my impl")
  println(cameraSurveillance(smallTree))
  println(cameraSurveillance(cameraTree))
  println(cameraSurveillance(danielTree))

  println("Daniel stack impl")
  println(cameraSurveillance_v2(smallTree))
  println(cameraSurveillance_v2(cameraTree))
  println(cameraSurveillance_v2(danielTree))

  println("Daniel tailrec impl")
  println(cameraSurveillance_v3(smallTree))
  println(cameraSurveillance_v3(cameraTree))
  println(cameraSurveillance_v3(danielTree))
}
