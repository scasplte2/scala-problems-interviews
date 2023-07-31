package com.rockthejvm.graphs

import scala.annotation.tailrec

object UniversityCourses extends App {

  /*
  nCourses is a number of courses at uni labeled 0 -> n - 1
  prereqs = List[(a,b)]
  (a,b) = b is required in order to take a

  Can you take all courses 0 -> n-1 without breaking any prereqs?
  -> reduces to trying to find a cycle in the graph
   */
  def canTakeAllCourses(nCourses: Int, prereqs: List[(Int, Int)]): Boolean = {
    val courseGraph = (0 until nCourses).map(c => (c, Set[Int]())).toMap

    val preReqGraph = prereqs.foldLeft(Map[Int, Set[Int]]()){ case (acc, (a,b)) =>
      acc + (a -> (acc.getOrElse(a, Set()) + b))
    }

    val graph = courseGraph ++ preReqGraph

    (0 until nCourses).forall(GraphProblems.findCycle(graph, _).isEmpty)
  }

  // topological sort
  def findOrder(nCourses: Int, prereqs: List[(Int, Int)]): List[Int] = {
    val graph = (0 until nCourses).map(c => (c, Set[Int]())).toMap ++
      prereqs.foldLeft(Map[Int, Set[Int]]()) { case (acc, (a, b)) =>
      acc + (b -> (acc.getOrElse(b, Set()) + a))
    }

    /*
    otr([0,1,2,3,4,5], [], [], [], []) ---------------------- start with list of all nodes
    otr([1,2,3,4,5], [0], [], [], []) ----------------------- if stack is empty pop remaining head node to stack
    otr([1,2,3,4,5], [2,3,0], [0], [], []) ------------------ if stack non empty, expand children, push to expanding
    otr([1,2,3,4,5], [3,0], [0], [2], [2]) ------------------ pop node from stack, if terminal add to order and visited
    otr([1,2,3,4,5], [0], [0], [2,3], [2,3]) ---------------- (3 is terminal so repeat above)
    otr([1,2,3,4,5], [], [], [2,3,0], [3,2]) ---------------- node in stack has been expanded, include in ordering
    otr([2,3,4,5], [1], [], [2,3,0], [0,3,2]) --------------- pop new head from remaining to stack
    otr([2,3,4,5], [0,4,1], [1], [2,3,0], [0,3,2]) ---------- expand children
    otr([2,3,4,5], [4,1], [1], [2,3,0], [0,3,2]) ------------ repeat the expansion process
    otr([2,3,4,5], [5,4,1], [1,4], [2,3,0], [0,3,2]) -------- since 4 is not terminal, expand it as well
    otr([2,3,4,5], [4,1], [1,4], [2,3,0,5], [5,0,3,2]) ------ continue adding to visited and ordering
    otr([2,3,4,5], [1], [1], [2,3,0,5,4], [4,5,0,3,2]) ------ 4 has been expanded, move to ordering
    otr([2,3,4,5], [], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ---- 1 expanded, move to ordering
    otr([3,4,5], [2], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ----- pop 2 from remaining, but already in visited, so skip
    otr([3,4,5], [], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ------ same as above (continue checking visited)
    otr([4,5], [3], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) -------
    otr([4,5], [], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) --------
    otr([5], [4], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ---------
    otr([5], [], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ----------
    otr([], [5], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ----------
    otr([], [], [], [2,3,0,5,4,1], [1,4,5,0,3,2]) ----------- once stack is empty and remaining is empty, return ordering

    Complexity: O(N) time, O(N) space
     */
    @tailrec
    def orderTailrec(
                      remaining: Set[Int],
                      stack: List[Int] = List(),
                      expanding: Set[Int] = Set(),
                      visited: Set[Int] = Set(),
                      order: List[Int] = List()
                    ): List[Int] = {
      if (stack.isEmpty){
        if (remaining.isEmpty) order
        else orderTailrec(remaining.tail, List(remaining.head), Set(), visited, order)
      } else {
        val node = stack.head

        if (visited.contains(node)) {
          // node is already in the final ordering
          orderTailrec(remaining, stack.tail, expanding, visited, order)
        } else if (expanding.contains(node)) {
          // node has already been expanded (DFS), include it in the ordering
          orderTailrec(remaining, stack.tail, expanding - node, visited + node, node :: order)
        } else {
          // expansion phase
          val coursesAfter = graph(node)
          if (coursesAfter.exists(expanding.contains)) List() // corresponds to a cycle in the graph
          else orderTailrec(remaining, coursesAfter.toList ++ stack, expanding + node, visited, order)
        }
      }
    }

    orderTailrec(graph.keySet)

  }

  println(canTakeAllCourses(4, List((2,0),(3,0),(3,1))))
  println(canTakeAllCourses(3, List((1,2),(2,0))))

  println(findOrder(4, List((2,0),(3,0),(3,1))))
}
